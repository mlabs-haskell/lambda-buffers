{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module KindChecker (getType) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Bifunctor (Bifunctor (second))

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Prettyprinter

type Atom = String
infixr 8 :->:

data Kind
  = Type
  | Kind :->: Kind
  | KVar String
  deriving (Eq, Show)

instance Pretty Kind where
  pretty = \case
    Type -> pretty "*"
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> pretty "→" <+> pretty z
    x :->: y -> pretty x <+> pretty "→" <+> pretty y
    KVar a -> pretty a

data Type
  = Var Atom
  | App Type Type
  | Abs String Type
  deriving (Eq, Show)

instance Pretty Type where
  pretty = \case
    Var a -> pretty a
    App t1 t2 -> show' t1 <> pretty " " <> show' t2
    Abs a t1 -> pretty "λ" <> pretty a <> pretty "." <> pretty t1
    where
      show' = \case
        Var a -> pretty a
        App t1 t2 -> parens $ show' t1 <+> show' t2
        Abs a t1 -> parens $ pretty "λ" <> pretty a <> pretty "." <> show' t1

data Context = Context
  { context :: [(Atom, Kind)]
  , addContext :: [(Atom, Kind)]
  }
  deriving (Show, Eq)

instance Pretty Context where
  pretty c = case addContext c of
    [] -> pretty "Γ"
    ctx -> pretty "Γ" <+> pretty "∪" <+> braces (setPretty ctx)
    where
      setPretty = hsep . punctuate comma . fmap (\(v, t) -> pretty v <> pretty ":" <+> pretty t)

-- | Utility to unify the two.
getAllContext :: Context -> [(Atom, Kind)]
getAllContext c = context c <> addContext c

newtype Judgement = Judgement {getJudgement :: (Context, Type, Kind)}
  deriving (Show, Eq)

instance Pretty Judgement where
  pretty (Judgement (c, t, k)) = pretty c <> pretty " ⊢ " <> pretty t <+> pretty ":" <+> pretty k

data Derivation
  = Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation
  deriving (Show, Eq)

instance Pretty Derivation where
  pretty x = case x of
    Axiom j -> hang 2 $ pretty j
    Abstraction j d -> dNest j [d]
    Application j d1 d2 -> dNest j [d1, d2]
    where
      dNest j ds = pretty j <> line <> hang 2 (encloseSep (lbracket <> space) rbracket (space <> pretty "∧" <> space) (pretty <$> ds))

type DError = String

newtype Constraint = Constraint (Kind, Kind)
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Constraint (t1, t2)) = pretty t1 <+> pretty "=" <+> pretty t2

newtype Substitution = Substitution {getSubstitution :: (Atom, Kind)}
  deriving (Show, Eq)

instance Pretty Substitution where
  pretty (Substitution (a, k)) = pretty a <+> pretty "↦" <+> pretty k

newtype DerivationContext = DC
  { freshVarStream :: [Atom]
  }

type DeriveM a = ReaderT Context (StateT DerivationContext (WriterT [Constraint] (Except DError))) a

type Derive a =
  forall m.
  ( MonadReader Context m
  , MonadState DerivationContext m
  , MonadWriter [Constraint] m
  , MonadError DError m
  ) =>
  m a

-- | Run derivation builder - not unified yet.
runDerive :: Type -> Either DError (Derivation, [Constraint])
runDerive t = runExcept $ runWriterT $ evalStateT (runReaderT (derive t) defContext) (DC atoms)

-- | Run derivation - throw error.
runDerive' :: Type -> (Derivation, [Constraint])
runDerive' t = either error id $ runExcept $ runWriterT $ evalStateT (runReaderT (derive t) defContext) (DC atoms)

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
      let ty1 = getKind d1
          ty2 = getKind d2
      v <- KVar <$> fresh
      tell [Constraint (ty1, ty2 :->: v)]
      pure $ Application (Judgement (c, x, v)) d1 d2
    Abs v t -> do
      newTy <- KVar <$> fresh
      d <- local (\(Context ctx addC) -> Context ctx $ (v, newTy) : addC) (derive t)
      let ty = getKind d
      freshT <- KVar <$> fresh
      tell [Constraint (freshT, newTy :->: ty)]
      pure $ Abstraction (Judgement (c, x, freshT)) d
  where
    fresh :: Derive Atom
    fresh = do
      (DC vs) <- get
      case vs of
        a : as -> put (DC as) >> pure a
        [] -> throwError "Impossible ~ end of infinite stream"

{- | Gets the binding from the context - if the variable is not bound throw an
 error.
-}
getBinding :: Atom -> Derive Kind
getBinding t = do
  ctx <- asks getAllContext
  case t `lookup` ctx of
    Just x -> pure x
    Nothing -> throwError $ "Unbound term:" <> show (pretty t)

-- | Gets kind from a derivation.
getKind :: Derivation -> Kind
getKind = \case
  Axiom (Judgement (_, _, k)) -> k
  Abstraction (Judgement (_, _, k)) _ -> k
  Application (Judgement (_, _, k)) _ _ -> k

-- | Unification error. :fixme:
type UErr = String

-- | Unification monad.
type Unifier a = Except UErr a

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
    nope c = throwError $ unlines ["Cannot unify: " <> show (pretty c)]

    appearsErr var ty =
      throwError $
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
runUnify :: [Constraint] -> Either UErr [Substitution]
runUnify = runExcept . unify

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

-- | Given a term (and the default context) - gives a type.
getType :: Type -> Either UErr Kind
getType t = do
  (d, c) <- runDerive t
  s <- runUnify c
  let res = foldl (flip substitute) d s
  pure $ getKind res

----------------------------------------------------------------------------------
-- Testing functions
-- :fixme: add tests, not this.

-- | For testing.
getType' :: Type -> IO ()
getType' t = do
  let (d, c) = runDerive' t
  print $ pretty (d, c)
  putStrLn ""
  let s = either error id $ runUnify c
  print $ pretty s
  putStrLn ""
  let res = go d s
  print $ pretty res
  putStrLn ""
  putStrLn $ show (pretty t) <> ":" <> (show . pretty $ getKind res)
  where
    go = foldl (flip substitute)

-- | Fresh atoms
atoms :: [Atom]
atoms = ['1' ..] >>= \y -> ['a' .. 'z'] >>= \x -> pure [x, y]

-- | Default context -- for testing.
defContext :: Context
defContext =
  Context
    { context =
        [ ("Either", Type :->: Type :->: Type)
        , ("Maybe", Type :->: Type)
        , ("(,)", Type :->: Type :->: Type)
        , ("Int", Type)
        , ("Bool", Type)
        , ("Map", Type :->: Type :->: Type)
        , ("List", Type :->: Type)
        , ("StateT", Type :->: (Type :->: Type) :->: Type :->: Type)
        ]
    , addContext = []
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
