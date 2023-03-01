{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This pragma^ is needed due to redundant constraint in Getters and Eff.

module LambdaBuffers.Compiler.KindCheck.Inference (
  Kind (..),
  Context (..),
  Atom,
  Type (..),
  infer,
  DeriveM,
  DeriveEff,
  context,
  addContext,
  InferErr (..),
  Constraint (..),
) where

import Control.Lens (view, (%~), (&), (.~), (^.))
import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, asks, local, runReader)
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldrM)
import Data.Map qualified as M
import Data.Text qualified as T
import LambdaBuffers.Compiler.KindCheck.Derivation (
  Context (Context),
  Derivation (Abstraction, Application, Axiom, Implication),
  Judgement (Judgement),
  addContext,
  context,
  d'kind,
  d'type,
 )
import LambdaBuffers.Compiler.KindCheck.Kind (Atom, Kind (KType, KVar, (:->:)))
import LambdaBuffers.Compiler.KindCheck.Type (
  Type (Abs, App, Constructor, Opaque, Product, Sum, UnitT, Var, VoidT),
  Variable (QualifiedTyRef, TyVar),
 )
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, mkInfoLess)
import LambdaBuffers.Compiler.ProtoCompat.Types (localRef2ForeignRef)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty), (<+>))

-- | Utility to unify the two.
getAllContext :: Context -> M.Map (InfoLess Variable) Kind
getAllContext c = c ^. context <> c ^. addContext

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

newtype DerivationContext = DC
  { _startAtom :: Atom
  }

type DeriveEff = '[State Context, State DerivationContext, State [Constraint], Error InferErr]

type DeriveM a = Eff DeriveEff a

type Derive a =
  forall effs.
  Members
    '[ Reader Context
     , Reader Kind
     , Reader PC.ModuleName
     , State DerivationContext
     , Writer [Constraint]
     , Error InferErr
     ]
    effs =>
  Eff effs a

--------------------------------------------------------------------------------
-- Runners

-- | Run derivation builder - not unified yet.
runDerive ::
  Context ->
  PC.TyAbs ->
  Kind ->
  PC.ModuleName ->
  Either InferErr (Derivation, [Constraint])
runDerive ctx t k localMod =
  run $
    runError $
      runWriter $
        evalState (DC startAtom) $
          runReader ctx $
            runReader k $
              runReader localMod $
                derive t

infer ::
  Context ->
  PC.TyDef ->
  Kind ->
  PC.ModuleName ->
  Either InferErr Kind
infer ctx t k localMod = do
  (d, c) <- runDerive (defContext <> ctx) (t ^. #tyAbs) k localMod
  s <- runUnify' c
  let res = foldl (flip substitute) d s
  pure $ res ^. d'kind

-- | Default KC Context.
defContext :: Context
defContext = mempty

--------------------------------------------------------------------------------
-- Implementation

-- | Creates the derivation
derive :: PC.TyAbs -> Derive Derivation
derive x = deriveTyAbs x
  where
    fresh :: Derive Atom
    fresh = do
      DC a <- get
      put (DC $ a + 1)
      pure a

    deriveTyAbs :: PC.TyAbs -> Derive Derivation
    deriveTyAbs tyabs = do
      case M.toList (tyabs ^. #tyArgs) of
        [] -> deriveTyBody (x ^. #tyBody)
        a@(_, ar) : as -> do
          let argK = protoKind2Kind (ar ^. #argKind)
          bodyK <- KVar <$> fresh
          ctx <- ask

          let newContext = ctx & addContext %~ (<> M.singleton (mkInfoLess (TyVar (ar ^. #argName))) argK)
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
        let ty = QualifiedTyRef . view (localRef2ForeignRef localModule) $ r
        v <- getKind ty
        c <- ask
        pure . Axiom $ Judgement c (Var ty) v
      PC.ForeignI r -> do
        let ty = QualifiedTyRef r
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
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

--------------------------------------------------------------------------------
-- Unification

-- | Unifies constraints and creates substitutions.
unify :: [Constraint] -> Unifier [Substitution]
unify [] = pure []
unify (constraint@(Constraint (l, r)) : xs) = case l of
  KType -> case r of
    KType -> unify xs
    (_ :->: _) -> nope constraint
    KVar v ->
      let sub = Substitution (v, KType)
       in (sub :) <$> unify (sub `substituteIn` xs)
  x :->: y -> case r of
    KType -> nope constraint
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
    nope :: forall eff a. (Member (Error InferErr) eff) => Constraint -> Eff eff a
    nope = throwError . InferUnifyTermErr

    appearsErr :: forall eff a. Member (Error InferErr) eff => Atom -> Kind -> Eff eff a
    appearsErr var ty =
      throwError $
        InferRecursiveSubstitutionErr $
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
    substituteIn s ((Constraint (lt, rt)) : cs) = Constraint (applySubstitution s lt, applySubstitution s rt) : substituteIn s cs

-- | Applies substitutions to a kind.
applySubstitution :: Substitution -> Kind -> Kind
applySubstitution s@(Substitution (a, t)) k = case k of
  KType -> KType
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

    applySubstitutionCtx subs c@(Context ctx addCtx) = case M.toList addCtx of
      [] -> c
      xs -> Context ctx $ M.fromList $ second (applySubstitution subs) <$> xs

-- FIXME(cstml) not avoiding any clashes

-- | Fresh startAtom
startAtom :: Atom
startAtom = 0

--  Convert from internal Kind to Proto Kind.
protoKind2Kind :: PC.Kind -> Kind
protoKind2Kind = \case
  PC.Kind k -> case k of
    PC.KindArrow k1 k2 -> protoKind2Kind k1 :->: protoKind2Kind k2
    PC.KindRef PC.KType -> KType
    PC.KindRef PC.KUnspecified -> KType -- unspecified kinds get inferred and unified
