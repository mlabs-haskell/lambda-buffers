{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This pragma^ is needed due to redundant constraint in Getter.
module LambdaBuffers.Compiler.KindCheck.Inference (
  Kind (..),
  Context (..),
  Atom,
  Type (..),
  infer,
  DeriveM,
  DeriveEff,
  InferErr (..),
  context,
  addContext,
) where

import Data.Bifunctor (Bifunctor (second))

import LambdaBuffers.Compiler.KindCheck.Atom
import LambdaBuffers.Compiler.KindCheck.Context
import LambdaBuffers.Compiler.KindCheck.Kind

import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, asks, local, runReader)
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)

import Control.Lens (Getter, to, (&), (.~), (^.))
import Data.Map qualified as M

import Prettyprinter (
  Doc,
  Pretty (pretty),
  encloseSep,
  hang,
  lbracket,
  line,
  parens,
  rbracket,
  space,
  (<+>),
 )

data Type
  = Var Var
  | App Type Type
  | Abs String Type
  deriving stock (Eq, Show)

instance Pretty Type where
  pretty = \case
    Var a -> pretty a
    App t1 t2 -> show' t1 <> " " <> show' t2
    Abs a t1 -> "λ" <> pretty a <> "." <> pretty t1
    where
      show' :: Type -> Doc ann
      show' = \case
        Var a -> pretty a
        App t1 t2 -> parens $ show' t1 <+> show' t2
        Abs a t1 -> parens $ "λ" <> pretty a <> "." <> show' t1

newtype Judgement = Judgement {getJudgement :: (Context, Type, Kind)}
  deriving stock (Show, Eq)

instance Pretty Judgement where
  pretty (Judgement (c, t, k)) = pretty c <> " ⊢ " <> pretty t <+> ":" <+> pretty k

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
      dNest j ds = pretty j <> line <> hang 2 (encloseSep (lbracket <> space) rbracket (space <> "∧" <> space) (pretty <$> ds))

data InferErr
  = Misc String
  | ImpossibleErr String
  | UnboundTermErr String
  | ImpossibleUnificationErr String
  | RecursiveSubstitutionErr String
  deriving stock (Show, Eq)

newtype Constraint = Constraint (Kind, Kind)
  deriving stock (Show, Eq)

instance Pretty Constraint where
  pretty (Constraint (t1, t2)) = pretty t1 <+> "=" <+> pretty t2

newtype Substitution = Substitution {getSubstitution :: (Atom, Kind)}
  deriving stock (Show, Eq)

instance Pretty Substitution where
  pretty (Substitution (a, k)) = pretty a <+> "↦" <+> pretty k

newtype DerivationContext = DC
  { _freshVarStream :: [Atom]
  }

type DeriveEff = '[State Context, State DerivationContext, State [Constraint], Error InferErr]

type DeriveM a = Eff DeriveEff a

type Derive a =
  forall effs.
  Members
    '[ Reader Context
     , State DerivationContext
     , Writer [Constraint]
     , Error InferErr
     ]
    effs =>
  Eff effs a

--------------------------------------------------------------------------------
-- Runners

-- | Run derivation builder - not unified yet.
runDerive :: Context -> Type -> Either InferErr (Derivation, [Constraint])
runDerive ctx t = run $ runError $ runWriter $ evalState (DC atoms) $ runReader ctx (derive t)

infer :: Context -> Type -> Either InferErr Kind
infer ctx t = do
  (d, c) <- runDerive (defTerms <> ctx) t
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. topKind
  where
    defTerms =
      mempty
        & context
          .~ M.fromList
            [ ("Either", Type :->: Type :->: Type)
            , ("()", Type)
            , ("Void", Type)
            , ("(,)", Type :->: Type :->: Type)
            ]

--------------------------------------------------------------------------------
-- Implementation

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
      d <- local (\(Context ctx addC) -> Context ctx $ M.insert v newTy addC) (derive t)
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
  case ctx M.!? t of
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

-- | Unification monad.
type Unifier a = forall effs. Member (Error InferErr) effs => Eff effs a

-- | Gets the variables of a type.
getVariables :: Kind -> [Atom]
getVariables = \case
  Type -> mempty
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

--------------------------------------------------------------------------------
-- Unification

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
    nope :: forall eff b a. (Member (Error InferErr) eff, Pretty b) => b -> Eff eff a
    nope c = throwError $ ImpossibleUnificationErr $ unlines ["Cannot unify: " <> show (pretty c)]

    appearsErr :: forall eff a. Member (Error InferErr) eff => String -> Kind -> Eff eff a
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
runUnify' :: [Constraint] -> Either InferErr [Substitution]
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

    applySubstitutionCtx subs c@(Context ctx addCtx) = case M.toList addCtx of
      [] -> c
      xs -> Context ctx $ M.fromList $ second (applySubstitution subs) <$> xs

-- :fixme: not avoiding any clashes

-- | Fresh atoms
atoms :: [Atom]
atoms = ['1' ..] >>= \y -> ['a' .. 'z'] >>= \x -> pure [x, y]
