{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This pragma^ is needed due to redundant constraint in Getters and Eff.

module LambdaBuffers.Compiler.KindCheck.Inference (
  -- * API functions
  infer,
  runClassDefCheck,

  -- * Types
  Kind (..),
  Context (..),
  Atom,
  Type (..),
  DeriveEff,
  InferErr (..),
  Constraint (..),

  -- * Utility functions
  protoKind2Kind,
) where

import Control.Lens ((%~), (&), (.~), (^.))
import Control.Lens.Iso (withIso)
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, asks, local, runReader)
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Foldable (foldrM, traverse_)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import LambdaBuffers.Compiler.KindCheck.Derivation (
  Context (Context),
  Derivation (Abstraction, Application, Axiom, Implication),
  Judgement (Judgement),
  addContext,
  d'kind,
  d'type,
  getAllContext,
 )
import LambdaBuffers.Compiler.KindCheck.Kind (Atom, Kind (KConstraint, KType, KVar, (:->:)))
import LambdaBuffers.Compiler.KindCheck.Type (
  Type (Abs, App, Constructor, Opaque, Product, Sum, UnitT, Var, VoidT),
  Variable (QualifiedTyClassRef, QualifiedTyRef, TyVar),
  fcrISOqtcr,
  ftrISOqtr,
  lcrISOftcr,
  lcrISOqtcr,
  ltrISOqtr,
 )
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (mkInfoLess)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty), (<+>))

--------------------------------------------------------------------------------
-- Types

data InferErr
  = InferUnboundTermErr Variable
  | InferUnifyTermErr Constraint
  | InferRecursiveSubstitutionErr T.Text
  | InferImpossibleErr T.Text

newtype Constraint = Constraint (Kind, Kind)
  deriving stock (Show, Eq)

instance Pretty Constraint where
  pretty (Constraint (t1, t2)) = pretty t1 <+> "=" <+> pretty t2

newtype Substitution = Substitution {getSubstitution :: (Atom, Kind)}
  deriving stock (Show, Eq)

instance Pretty Substitution where
  pretty (Substitution (a, k)) = pretty a <+> "↦" <+> pretty k

--------------------------------------------------------------------------------
-- Effects

newtype DerivationContext = DC {_startAtom :: Atom}

type DeriveEff =
  '[Reader Context, Reader PC.ModuleName, State DerivationContext, Writer [Constraint], Error InferErr]

type Derive a = forall effs. Members DeriveEff effs => Eff effs a

--------------------------------------------------------------------------------
-- Runners

-- | Run Derive Monad - not unified.
runDerive :: Context -> PC.ModuleName -> Derive a -> Either InferErr (a, [Constraint])
runDerive ctx localMod =
  run . runError . runWriter . evalState (DC startAtom) . runReader ctx . runReader localMod

infer :: Context -> PC.TyDef -> PC.ModuleName -> Either InferErr Kind
infer ctx t localMod = do
  (d, c) <- runDerive ctx localMod (deriveTyAbs (t ^. #tyAbs))
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. d'kind

--------------------------------------------------------------------------------
-- Implementation

fresh :: Derive Atom
fresh = do
  DC a <- get
  put (DC $ a + 1)
  pure a

deriveTyAbs :: PC.TyAbs -> Derive Derivation
deriveTyAbs tyabs = do
  case M.toList (tyabs ^. #tyArgs) of
    [] -> deriveTyBody (tyabs ^. #tyBody)
    a@(n, ar) : as -> do
      let argK = protoKind2Kind (ar ^. #argKind)
      bodyK <- KVar <$> fresh
      ctx <- ask

      let newContext = ctx & addContext %~ (<> M.singleton (mkInfoLess (TyVar n)) argK)
      let newAbs = tyabs & #tyArgs .~ uncurry M.singleton a
      let restAbs = tyabs & #tyArgs .~ M.fromList as

      restF <- local (const newContext) $ deriveTyAbs restAbs

      let uK = restF ^. d'kind
      tell [Constraint (bodyK, uK)]
      pure $ Abstraction (Judgement ctx (Abs newAbs) (argK :->: bodyK)) restF

deriveTyBody :: PC.TyBody -> Derive Derivation
deriveTyBody = \case
  PC.OpaqueI si -> do
    ctx <- ask
    pure $ Axiom $ Judgement ctx (Opaque si) KType
  PC.SumI s -> deriveSum s

deriveSum :: PC.Sum -> Derive Derivation
deriveSum s = do
  case M.toList (s ^. #constructors) of
    [] -> voidDerivation
    c : cs -> do
      dc <- deriveConstructor $ snd c
      restDc <- deriveSum $ s & #constructors .~ M.fromList cs
      sumDerivation dc restDc

deriveConstructor :: PC.Constructor -> Derive Derivation
deriveConstructor c = do
  ctx <- ask
  d <- deriveProduct (c ^. #product)
  tell $ Constraint <$> [(KType, d ^. d'kind)]
  pure $ Implication (Judgement ctx (Constructor c) (d ^. d'kind)) d

deriveProduct :: PC.Product -> Derive Derivation
deriveProduct = \case
  PC.RecordI r -> deriveRecord r
  PC.TupleI t -> deriveTuple t

deriveRecord :: PC.Record -> Derive Derivation
deriveRecord r = do
  case M.toList (r ^. #fields) of
    [] -> unitDerivation
    f : fs -> do
      d1 <- deriveField $ snd f
      d2 <- deriveRecord $ r & #fields .~ M.fromList fs
      productDerivation d1 d2

deriveField :: PC.Field -> Derive Derivation
deriveField f = deriveTy $ f ^. #fieldTy

deriveTy :: PC.Ty -> Derive Derivation
deriveTy = \case
  PC.TyVarI tv -> deriveTyVar tv
  PC.TyAppI ta -> deriveTyApp ta
  PC.TyRefI tr -> deriveTyRef tr

deriveTyRef :: PC.TyRef -> Derive Derivation
deriveTyRef = \case
  PC.LocalI r -> do
    localModule <- ask
    let ty = QualifiedTyRef . withIso ltrISOqtr const $ (r, localModule)
    v <- getKind ty
    c <- ask
    pure . Axiom $ Judgement c (Var ty) v
  PC.ForeignI r -> do
    let ty = QualifiedTyRef . withIso ftrISOqtr const $ r
    v <- getKind ty
    c <- ask
    pure . Axiom $ Judgement c (Var ty) v

deriveTyVar :: PC.TyVar -> Derive Derivation
deriveTyVar tv = do
  let varName = tv ^. #varName
  v <- getKind $ TyVar varName
  c <- ask
  pure . Axiom $ Judgement c (Var $ TyVar varName) v

deriveTyApp :: PC.TyApp -> Derive Derivation
deriveTyApp ap = do
  f <- deriveTy (ap ^. #tyFunc)
  args <- deriveTy `traverse` (ap ^. #tyArgs)
  applyDerivation f args

deriveTuple :: PC.Tuple -> Derive Derivation
deriveTuple t = do
  voidD <- voidDerivation
  ds <- deriveTy `traverse` (t ^. #fields)
  foldrM productDerivation voidD ds

voidDerivation :: Derive Derivation
voidDerivation = (\ctx -> Axiom $ Judgement ctx VoidT KType) <$> ask

unitDerivation :: Derive Derivation
unitDerivation = (\ctx -> Axiom $ Judgement ctx UnitT KType) <$> ask

productDerivation :: Derivation -> Derivation -> Derive Derivation
productDerivation d1 d2 = do
  ctx <- ask
  let t1 = d1 ^. d'type
  let t2 = d2 ^. d'type
  tell $ Constraint <$> [(d1 ^. d'kind, KType), (d2 ^. d'kind, KType)]
  pure $ Application (Judgement ctx (Product t1 t2) KType) d1 d2

sumDerivation :: Derivation -> Derivation -> Derive Derivation
sumDerivation d1 d2 = do
  ctx <- ask
  let t1 = d1 ^. d'type
  let t2 = d2 ^. d'type
  tell $ Constraint <$> [(d1 ^. d'kind, KType), (d2 ^. d'kind, KType)]
  pure $ Application (Judgement ctx (Sum t1 t2) KType) d1 d2

applyDerivation :: Derivation -> [Derivation] -> Derive Derivation
applyDerivation d1 = \case
  [] -> pure d1
  d : ds -> do
    c <- ask
    d2 <- applyDerivation d ds
    v <- KVar <$> fresh
    tell [Constraint ((d2 ^. d'kind) :->: v, d1 ^. d'kind)]
    pure $ Application (Judgement c (App (d ^. d'type) (d2 ^. d'type)) v) d1 d2

--------------------------------------------------------------------------------
-- Class Checking

runClassDefCheck :: Context -> PC.ModuleName -> PC.ClassDef -> Either InferErr ()
runClassDefCheck ctx modName classDef = do
  (_, c) <- runDerive ctx modName $ deriveClassDef classDef
  void $ runUnify' c

-- | Checks the class definition for correct typedness.
deriveClassDef :: PC.ClassDef -> Derive ()
deriveClassDef classDef = traverse_ deriveConstraint (classDef ^. #supers)

deriveConstraint :: PC.Constraint -> Derive Derivation
deriveConstraint constraint = do
  mn <- ask
  ctx <- ask
  let qcr = case constraint ^. #classRef of
        PC.LocalCI lcr -> QualifiedTyClassRef . withIso lcrISOqtcr const $ (lcr, mn)
        PC.ForeignCI fcr -> QualifiedTyClassRef . withIso fcrISOqtcr const $ fcr
  dConstraint <- deriveVar qcr
  argD <- deriveTy (constraint ^. #argument)
  let argTy = argD ^. d'type
  freshK <- KVar <$> fresh
  tell [Constraint (dConstraint ^. d'kind, (argD ^. d'kind) :->: freshK)]
  pure $ Application (Judgement ctx (App (dConstraint ^. d'type) argTy) freshK) dConstraint argD

deriveVar :: Variable -> Derive Derivation
deriveVar v = do
  ctx <- ask
  k <- getKind v
  pure . Axiom $ Judgement ctx (Var v) k

--------------------------------------------------------------------------------
--

{- | Gets the binding from the context - if the variable is not bound throw an
 error.
-}
getKind :: Variable -> Derive Kind
getKind t = do
  ctx <- asks getAllContext
  case ctx M.!? mkInfoLess t of
    Just x -> pure x
    Nothing -> throwError $ InferUnboundTermErr t

-- | Unification monad.
type Unifier a = forall effs. Member (Error InferErr) effs => Eff effs a

-- | Gets the variables of a type.
getVariables :: Kind -> [Atom]
getVariables = \case
  KType -> mempty
  KConstraint -> mempty
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

--------------------------------------------------------------------------------
-- Unification

-- | Unifies constraints and creates substitutions.
unify :: [Constraint] -> Unifier [Substitution]
unify [] = pure []
unify (constraint@(Constraint (l, r)) : xs) = case l of
  -- Constants
  KType -> case r of
    KType -> unify xs
    KConstraint -> nope constraint
    (_ :->: _) -> nope constraint
    KVar v ->
      let sub = Substitution (v, KType)
       in (sub :) <$> unify (sub `substituteIn` xs)
  KConstraint -> case r of
    KType -> nope constraint
    KConstraint -> unify xs
    (_ :->: _) -> nope constraint
    KVar v ->
      let sub = Substitution (v, KConstraint)
       in (sub :) <$> unify (sub `substituteIn` xs)
  -- Arrows
  x :->: y -> case r of
    KType -> nope constraint
    KConstraint -> nope constraint
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
  -- Variables
  KVar a -> case r of
    KVar b ->
      if a == b
        then unify xs
        else
          let sub = Substitution (a, r)
           in (sub :) <$> unify (sub `substituteIn` xs)
    _ -> unify $ Constraint (r, l) : xs
  where
    nope :: forall eff a. (Member (Error InferErr) eff) => Constraint -> Eff eff a
    nope = throwError . InferUnifyTermErr

    appearsErr :: forall eff a. Member (Error InferErr) eff => Atom -> Kind -> Eff eff a
    appearsErr var ty =
      throwError
        $ InferRecursiveSubstitutionErr
          . mconcat
        $ ["Cannot unify: ", p var, " with ", p ty, ". ", p var, " appears in: ", p ty, "."]
      where
        p :: forall b. Pretty b => b -> Text
        p = T.pack . show . pretty

    appearsIn a ty = a `elem` getVariables ty

    substituteIn _ [] = []
    substituteIn s ((Constraint (lt, rt)) : cs) = Constraint (applySubstitution s lt, applySubstitution s rt) : substituteIn s cs

-- | Applies substitutions to a kind.
applySubstitution :: Substitution -> Kind -> Kind
applySubstitution s@(Substitution (a, t)) k = case k of
  KType -> KType
  KConstraint -> KConstraint
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
  Implication j dc -> Implication (applySubsToJudgement s j) (substitute s dc)
  where
    applySubsToJudgement sub (Judgement ctx t k) = Judgement (applySubstitutionCtx s ctx) t (applySubstitution sub k)

    applySubstitutionCtx subs ctx = ctx & addContext %~ fmap (applySubstitution subs)

-- | Fresh startAtom
startAtom :: Atom
startAtom = 0

--  Convert from internal Kind to Proto Kind.
protoKind2Kind :: PC.Kind -> Kind
protoKind2Kind = \case
  PC.Kind k -> case k of
    PC.KindArrow k1 k2 -> protoKind2Kind k1 :->: protoKind2Kind k2
    PC.KindRef PC.KType -> KType
    PC.KindRef PC.KUnspecified -> KType
    PC.KindRef PC.KConstraint -> KConstraint
