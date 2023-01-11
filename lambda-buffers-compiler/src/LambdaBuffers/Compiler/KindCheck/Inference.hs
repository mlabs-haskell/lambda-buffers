{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LambdaBuffers.Compiler.KindCheck.Inference (
  getType,
  Kind (..),
  Context (..),
  Atom,
  Type (..),
  infer,
  DeriveM,
  DeriveEff,
  DError,
  context,
  addContext,
) where

import Data.Bifunctor (Bifunctor (second))

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

import Control.Lens (Getter, makeLenses, to, (^.))

import Prettyprinter

type Atom = String
infixr 8 :->:

data Kind
  = Type
  | Kind :->: Kind
  | KVar String
  deriving stock (Eq, Show)

instance Pretty Kind where
  pretty = \case
    Type -> pretty @String "*"
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> pretty @String "→" <+> pretty z
    x :->: y -> pretty x <+> pretty @String "→" <+> pretty y
    KVar a -> pretty a

data Type
  = Var Atom
  | App Type Type
  | Abs String Type
  deriving stock (Eq, Show)

instance Pretty Type where
  pretty = \case
    Var a -> pretty a
    App t1 t2 -> show' t1 <> pretty @String " " <> show' t2
    Abs a t1 -> pretty @String "λ" <> pretty a <> pretty @String "." <> pretty t1
    where
      show' :: Type -> Doc ann
      show' = \case
        Var a -> pretty a
        App t1 t2 -> parens $ show' t1 <+> show' t2
        Abs a t1 -> parens $ pretty @String "λ" <> pretty a <> pretty @String "." <> show' t1

data Context = Context
  { _context :: [(Atom, Kind)]
  , _addContext :: [(Atom, Kind)]
  }
  deriving stock (Show, Eq)

makeLenses ''Context

instance Pretty Context where
  pretty c = case c ^. addContext of
    [] -> pretty @String "Γ"
    ctx -> pretty @String "Γ" <+> pretty @String "∪" <+> braces (setPretty ctx)
    where
      setPretty :: [(Atom, Kind)] -> Doc ann
      setPretty = hsep . punctuate comma . fmap (\(v, t) -> pretty v <> pretty @String ":" <+> pretty t)

instance Semigroup Context where
  (Context a1 b1) <> (Context a2 b2) = Context (a1 <> a2) (b1 <> b2)

instance Monoid Context where
  mempty = Context mempty mempty

-- | Utility to unify the two.
getAllContext :: Context -> [(Atom, Kind)]
getAllContext c = c ^. context <> c ^. addContext

newtype Judgement = Judgement {getJudgement :: (Context, Type, Kind)}
  deriving stock (Show, Eq)

instance Pretty Judgement where
  pretty (Judgement (c, t, k)) = pretty c <> pretty @String " ⊢ " <> pretty t <+> pretty @String ":" <+> pretty k

data Derivation
  = Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation
  deriving stock (Show, Eq)

instance Pretty Derivation where
  pretty x = case x of
    Axiom j -> hang 2 $ pretty j
    Abstraction j d -> dNest j [d]
    Application j d1 d2 -> dNest j [d1, d2]
    where
      dNest :: forall a b c. (Pretty a, Pretty b) => a -> [b] -> Doc c
      dNest j ds = pretty j <> line <> hang 2 (encloseSep (lbracket <> space) rbracket (space <> pretty @String "∧" <> space) (pretty <$> ds))

data DError
  = Misc String
  | ImpossibleErr String
  | UnboundTermErr String
  | ImpossibleUnificationErr String
  | RecursiveSubstitutionErr String
  deriving stock (Show, Eq)

newtype Constraint = Constraint (Kind, Kind)
  deriving stock (Show, Eq)

instance Pretty Constraint where
  pretty (Constraint (t1, t2)) = pretty t1 <+> pretty @String "=" <+> pretty t2

newtype Substitution = Substitution {getSubstitution :: (Atom, Kind)}
  deriving stock (Show, Eq)

instance Pretty Substitution where
  pretty (Substitution (a, k)) = pretty a <+> pretty @String "↦" <+> pretty k

newtype DerivationContext = DC
  { freshVarStream :: [Atom]
  }

type DeriveEff = '[State Context, State DerivationContext, State [Constraint], Error DError]

type DeriveM a = Eff DeriveEff a

type Derive a =
  forall effs.
  Members
    '[ Reader Context
     , State DerivationContext
     , Writer [Constraint]
     , Error DError
     ]
    effs =>
  Eff effs a

-- | Run derivation builder - not unified yet.
runDerive :: Type -> Either DError (Derivation, [Constraint])
runDerive = runDerive'' defContext

-- | Run derivation - throw error.
runDerive' :: Type -> (Derivation, [Constraint])
runDerive' t = either (error.show) id $ runDerive t

-- | Run derivation builder - not unified yet.
runDerive'' :: Context -> Type -> Either DError (Derivation, [Constraint])
runDerive'' ctx t = run $ runError $ runWriter $ evalState (DC atoms) $ runReader ctx (derive t)

infer :: Context -> Type -> Either DError Kind
infer ctx t = do
  (d, c) <- runDerive'' ctx t
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. topKind

-- | Creates the derivation
derive :: Type -> Derive Derivation
derive x = do
  c <- ask
  case x of
    Var at -> do
      v <- getBinding at
      pure $ Axiom $ Judgement (c, x, v)
    App t1 t2 -> do
      d1 <- derive t1
      d2 <- derive t2
      let ty1 = d1 ^. topKind
          ty2 = d2 ^. topKind
      v <- KVar <$> fresh
      tell [Constraint (ty1, ty2 :->: v)]
      pure $ Application (Judgement (c, x, v)) d1 d2
    Abs v t -> do
      newTy <- KVar <$> fresh
      d <- local (\(Context ctx addC) -> Context ctx $ (v, newTy) : addC) (derive t)
      let ty = d ^. topKind
      freshT <- KVar <$> fresh
      tell [Constraint (freshT, newTy :->: ty)]
      pure $ Abstraction (Judgement (c, x, freshT)) d
  where
    fresh :: Derive Atom
    fresh = do
      (DC vs) <- get
      case vs of
        a : as -> put (DC as) >> pure a
        [] -> throwError $ ImpossibleErr "End of infinite stream"

{- | Gets the binding from the context - if the variable is not bound throw an
 error.
-}
getBinding :: Atom -> Derive Kind
getBinding t = do
  ctx <- asks getAllContext
  case t `lookup` ctx of
    Just x -> pure x
    Nothing -> throwError $ UnboundTermErr $ show (pretty t)

-- | Gets kind from a derivation.
topKind :: Getter Derivation Kind
topKind = to f
  where
    f = \case
      Axiom (Judgement (_, _, k)) -> k
      Abstraction (Judgement (_, _, k)) _ -> k
      Application (Judgement (_, _, k)) _ _ -> k

type UErr = DError

-- | Unification monad.
type Unifier a = forall effs. Member (Error UErr) effs => Eff effs a

-- | Gets the variables of a type.
getVariables :: Kind -> [Atom]
getVariables = \case
  Type -> mempty
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

-- | Unifies constraints and creates substitutions.
unify :: [Constraint] -> Unifier [Substitution]
unify [] = pure []
unify (constraint@(Constraint (l, r)) : xs) = case l of
  Type -> case r of
    Type -> unify xs
    (_ :->: _) -> nope constraint
    KVar v ->
      let sub = Substitution (v, Type)
       in (sub :) <$> unify (sub `substituteIn` xs)
  x :->: y -> case r of
    Type -> nope constraint
    KVar v ->
      if v `appearsIn` l
        then appearsErr v l
        else
          let sub = Substitution (v, l)
           in (sub :) <$> unify (sub `substituteIn` xs)
    m :->: n ->
      let c1 = Constraint (x, m)
          c2 = Constraint (y, n)
       in unify (c1 : c2 : xs)
  KVar a -> case r of
    KVar b ->
      if a == b
        then unify xs
        else
          let sub = Substitution (a, r)
           in (sub :) <$> unify (sub `substituteIn` xs)
    _ -> unify $ Constraint (r, l) : xs
  where
    nope c = throwError $ ImpossibleUnificationErr $ unlines ["Cannot unify: " <> show (pretty c)]

    appearsErr var ty =
      throwError $
        RecursiveSubstitutionErr $
          mconcat
            [ "Cannot unify: "
            , show (pretty var)
            , " with "
            , show (pretty ty)
            , ". "
            , show (pretty var)
            , " appears in: "
            , show (pretty ty)
            , "."
            ]

    appearsIn a ty = a `elem` getVariables ty

    substituteIn _ [] = []
    substituteIn s ((Constraint (lt, rt)) : cs) = Constraint (applySubstitution s lt, applySubstitution s rt) : substituteIn s cs

-- | Applies substitutions to a kind.
applySubstitution :: Substitution -> Kind -> Kind
applySubstitution s@(Substitution (a, t)) k = case k of
  Type -> Type
  l :->: r -> applySubstitution s l :->: applySubstitution s r
  KVar v -> if v == a then t else k

-- | Runs the unifier.
runUnify :: forall effs. [Constraint] -> Eff effs (Either UErr [Substitution])
runUnify = runError . unify

-- | Runs the unifier.
runUnify' :: [Constraint] -> Either UErr [Substitution]
runUnify' = run . runError . unify

{- | Applies substitutions to all the types in the Derivation, and the
 additional context.
-}
substitute :: Substitution -> Derivation -> Derivation
substitute s d = case d of
  Axiom j -> Axiom (applySubsToJudgement s j)
  Abstraction j dc -> Abstraction (applySubsToJudgement s j) (substitute s dc)
  Application j d1 d2 -> Application (applySubsToJudgement s j) (substitute s d1) (substitute s d2)
  where
    applySubsToJudgement sub (Judgement (ctx, t, k)) = Judgement (applySubstitutionCtx s ctx, t, applySubstitution sub k)

    applySubstitutionCtx subs c@(Context ctx addCtx) = case addCtx of
      [] -> c
      xs -> Context ctx $ second (applySubstitution subs) <$> xs

-- | Given a term (and the default context) - gives a Kind.
getType :: Type -> Either UErr Kind
getType t = do
  (d, c) <- runDerive t
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. topKind

----------------------------------------------------------------------------------
-- Testing functions
-- :fixme: add tests, not this.

-- | For testing.
getType' :: Type -> IO ()
getType' t = do
  let (d, c) = runDerive' t
  print $ pretty (d, c)
  putStrLn ""
  let s = either (error.show) id $ runUnify' c
  print $ pretty s
  putStrLn ""
  let res = go d s
  print $ pretty res
  putStrLn ""
  putStrLn $ show (pretty t) <> ":" <> (show . pretty $ res ^. topKind)
  where
    go = foldl (flip substitute)

-- | Fresh atoms
atoms :: [Atom]
atoms = ['1' ..] >>= \y -> ['a' .. 'z'] >>= \x -> pure [x, y]

-- | Default context -- for testing.
defContext :: Context
defContext =
  Context
    { _context =
        [ ("Either", Type :->: Type :->: Type)
        , ("Maybe", Type :->: Type)
        , ("(,)", Type :->: Type :->: Type)
        , ("Int", Type)
        , ("Bool", Type)
        , ("Map", Type :->: Type :->: Type)
        , ("List", Type :->: Type)
        , ("StateT", Type :->: (Type :->: Type) :->: Type :->: Type)
        , ("Opaque", Type)
        , ("Voie", Type)
        ]
    , _addContext = []
    }

-- >>> getType' tterm
-- Cannot unify: * → b1 = *
tterm = App (App (Abs "b" (App (Var "b") (Var "Int"))) (Var "Int")) (App (Var "Int") (Var "Int"))

-- >>> getType tterm2
-- Right Type
tterm2 = Var "Int"

-- >>> getType tterm3
-- Right (KVar "a1" :->: ((Type :->: KVar "c1") :->: KVar "c1"))
tterm3 = Abs "y" $ Abs "x" $ App (Var "x") (Var "Int")

-- >>> getType tterm4
-- Cannot unify: * → b1 = *
tterm4 = App (Abs "x" $ App (Var "x") (Var "Int")) (Var "Int")

-- >>> getType tterm5
-- Right Type
tterm5 = App (App (Abs "F" $ Abs "a" $ App (Var "F") (App (Var "F") (Var "a"))) (Var "Maybe")) (Var "Int")

-- >>>  getType tterm6
-- Right ((KVar "d1" :->: KVar "d1") :->: (KVar "d1" :->: KVar "d1"))
tterm6 = Abs "F" $ Abs "a" $ App (Var "F") (App (Var "F") (Var "a"))

-- >>>  getType tterm7
-- Right Type
tterm7 = App (App (App (Var "StateT") (Var "Int")) (Var "Maybe")) (Var "Int")
