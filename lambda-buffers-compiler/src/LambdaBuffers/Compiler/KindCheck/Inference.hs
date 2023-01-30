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

import LambdaBuffers.Compiler.KindCheck.Context (Context (Context), addContext, context, getAllContext)
import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KVar, KindP, (:->:)), KindPrimitive (Constraint, Type))
import LambdaBuffers.Compiler.KindCheck.Type (Type (Abs, App, Var))
import LambdaBuffers.Compiler.KindCheck.Variable (Atom)

import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, asks, local, runReader)
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)

import Data.String (fromString)
import Data.Text qualified as T

import Control.Lens (Getter, to, (&), (.~), (^.))
import Data.Map qualified as M

import Prettyprinter (
  Doc,
  Pretty (pretty),
  encloseSep,
  hang,
  lbracket,
  line,
  rbracket,
  space,
  (<+>),
 )

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
  = Misc T.Text
  | ImpossibleErr T.Text
  | UnboundTermErr T.Text
  | ImpossibleUnificationErr T.Text
  | RecursiveSubstitutionErr T.Text
  deriving stock (Show, Eq)

newtype UConstraint = UConstraint (Kind, Kind)
  deriving stock (Show, Eq)

instance Pretty UConstraint where
  pretty (UConstraint (t1, t2)) = pretty t1 <+> "=" <+> pretty t2

newtype Substitution = Substitution {getSubstitution :: (Atom, Kind)}
  deriving stock (Show, Eq)

instance Pretty Substitution where
  pretty (Substitution (a, k)) = pretty a <+> "↦" <+> pretty k

newtype DerivationContext = DC
  { _freshVarStream :: [Atom]
  }

type DeriveEff = '[State Context, State DerivationContext, State [UConstraint], Error InferErr]

type DeriveM a = Eff DeriveEff a

type Derive a =
  forall effs.
  Members
    '[ Reader Context
     , State DerivationContext
     , Writer [UConstraint]
     , Error InferErr
     ]
    effs =>
  Eff effs a

--------------------------------------------------------------------------------
-- Runners

-- | Run derivation builder - not unified yet.
runDerive :: Context -> Type -> Either InferErr (Derivation, [UConstraint])
runDerive ctx t = run $ runError $ runWriter $ evalState (DC atoms) $ runReader ctx (derive t)

infer :: Context -> Type -> Either InferErr Kind
infer ctx t = do
  (d, c) <- runDerive (defContext <> ctx) t
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. topKind

defContext :: Context
defContext =
  mempty
    & context
      .~ M.fromList
        [ ("Σ", KindP Type :->: KindP Type :->: KindP Type)
        , ("Π", KindP Type :->: KindP Type :->: KindP Type)
        , ("𝟙", KindP Type)
        , ("𝟘", KindP Type)
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
      tell [UConstraint (ty1, ty2 :->: v)]
      pure $ Application (Judgement (c, x, v)) d1 d2
    Abs v t -> do
      newTy <- KVar <$> fresh
      d <- local (\(Context ctx addC) -> Context ctx $ M.insert v newTy addC) (derive t)
      let ty = d ^. topKind
      freshT <- KVar <$> fresh
      tell [UConstraint (freshT, newTy :->: ty)]
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
    Nothing -> throwError $ UnboundTermErr $ (T.pack . show . pretty) t

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
  KindP _ -> mempty
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

--------------------------------------------------------------------------------
-- Unification

-- | Unifies constraints and creates substitutions.
unify :: [UConstraint] -> Unifier [Substitution]
unify [] = pure []
unify (constraint@(UConstraint (l, r)) : xs) = case l of
  KindP Type -> case r of
    KindP Type -> unify xs
    KVar v ->
      let sub = Substitution (v, KindP Type)
       in (sub :) <$> unify (sub `substituteIn` xs)
    (_ :->: _) -> nope constraint
    KindP _ -> nope constraint
  KindP Constraint -> case r of
    KindP Type -> unify xs
    KVar v ->
      let sub = Substitution (v, KindP Constraint)
       in (sub :) <$> unify (sub `substituteIn` xs)
    (_ :->: _) -> nope constraint
    KindP _ -> nope constraint
  x :->: y -> case r of
    KindP _ -> nope constraint
    KVar v ->
      if v `appearsIn` l
        then appearsErr v l
        else
          let sub = Substitution (v, l)
           in (sub :) <$> unify (sub `substituteIn` xs)
    m :->: n ->
      let c1 = UConstraint (x, m)
          c2 = UConstraint (y, n)
       in unify (c1 : c2 : xs)
  KVar a -> case r of
    KVar b ->
      if a == b
        then unify xs
        else
          let sub = Substitution (a, r)
           in (sub :) <$> unify (sub `substituteIn` xs)
    _ -> unify $ UConstraint (r, l) : xs
  where
    nope :: forall eff b a. (Member (Error InferErr) eff, Pretty b) => b -> Eff eff a
    nope c = throwError . ImpossibleUnificationErr . T.unlines $ ["Cannot unify: " <> (T.pack . show . pretty) c]

    appearsErr :: forall eff a. Member (Error InferErr) eff => T.Text -> Kind -> Eff eff a
    appearsErr var ty =
      throwError $
        RecursiveSubstitutionErr $
          mconcat
            [ "Cannot unify: "
            , T.pack . show . pretty $ var
            , " with "
            , T.pack . show . pretty $ ty
            , ". "
            , T.pack . show . pretty $ var
            , " appears in: "
            , T.pack . show . pretty $ ty
            , "."
            ]

    appearsIn a ty = a `elem` getVariables ty

    substituteIn _ [] = []
    substituteIn s ((UConstraint (lt, rt)) : cs) = UConstraint (applySubstitution s lt, applySubstitution s rt) : substituteIn s cs

-- | Applies substitutions to a kind.
applySubstitution :: Substitution -> Kind -> Kind
applySubstitution s@(Substitution (a, t)) k = case k of
  KindP Type -> KindP Type
  KindP Constraint -> KindP Constraint
  l :->: r -> applySubstitution s l :->: applySubstitution s r
  KVar v -> if v == a then t else k

-- | Runs the unifier.
runUnify' :: [UConstraint] -> Either InferErr [Substitution]
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
atoms = ['1' ..] >>= \y -> ['a' .. 'z'] >>= \x -> pure . fromString $ [x, y]
