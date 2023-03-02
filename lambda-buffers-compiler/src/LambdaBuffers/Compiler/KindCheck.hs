{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck (
  -- * Kind checking functions.
  check,
  check_,

  -- * Tested functions
  foldWithArrowToType,
) where

import Control.Lens (view, (&), (.~), (^.))
import Control.Lens.Iso (withIso)
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, interpret, reinterpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.TH (makeEffect)
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList), traverse_)
import Data.Map qualified as M
import LambdaBuffers.Compiler.KindCheck.Derivation (Context, constraintContext, context)
import LambdaBuffers.Compiler.KindCheck.Inference (protoKind2Kind)
import LambdaBuffers.Compiler.KindCheck.Inference qualified as I
import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KConstraint, KType, KVar, (:->:)))
import LambdaBuffers.Compiler.KindCheck.Type (
  Variable (QualifiedTyClassRef, QualifiedTyRef, TyVar),
  ftrISOqtr,
  ltrISOqtr,
  qTyRef'moduleName,
 )
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, mkInfoLess)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
type CompilerErr = PC.CompilerError

type Err = Error CompilerErr

-- | Main interface to the Kind Checker.
data Check a where
  KCheck :: PC.CompilerInput -> Check Context

makeEffect ''Check

-- | Interactions that happen at the level of the Global Checker.
data GlobalCheck a where
  CreateContext :: PC.CompilerInput -> GlobalCheck Context
  ValidateModule :: Context -> PC.Module -> GlobalCheck ()

makeEffect ''GlobalCheck

-- | Interactions that happen at the level of the
data ModuleCheck a where -- Module
  KCTypeDefinition :: PC.ModuleName -> Context -> PC.TyDef -> ModuleCheck Kind
  KCClassInstance :: PC.ModuleName -> Context -> PC.ClassDef -> ModuleCheck ()

--  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  GetSpecifiedKind :: PC.ModuleName -> PC.TyDef -> KindCheck Kind
  InferTypeKind :: PC.ModuleName -> PC.TyDef -> Context -> Kind -> KindCheck Kind
  CheckClassDefinition :: PC.ModuleName -> PC.ClassDef -> Context -> KindCheck ()
  CheckKindConsistency :: PC.ModuleName -> PC.TyDef -> Context -> Kind -> KindCheck Kind

--  CheckClassInstance :: PC.ModuleName -> KindCheck Kind

makeEffect ''KindCheck

--------------------------------------------------------------------------------
-- Runners

-- | The Check effect runner.
runCheck :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck =
  run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

{- | Run the check - return the validated context or the failure. The main API
 function of the library.
-}
check :: PC.CompilerInput -> PC.CompilerOutput
check = fmap (const PC.CompilerResult) . runCheck . kCheck

-- | Run the check - drop the result if it succeeds - useful for testing.
check_ :: PC.CompilerInput -> Either CompilerErr ()
check_ = void . runCheck . kCheck

--------------------------------------------------------------------------------
-- Transformations

{- | A transformation (in the context of the Kind Checker) is a mapping from one
 Effect to another. All effects can fail via the `Err` effect - which is
 essentially the Kind Check failure.
-}
type Transform x y =
  forall effs {a}. Member Err effs => Eff (x ': effs) a -> Eff (y ': effs) a

-- Transformation strategies
globalStrategy :: Transform Check GlobalCheck
globalStrategy = reinterpret $ \case
  KCheck ci -> do
    ctx <- createContext ci
    void $ validateModule ctx `traverse` (ci ^. #modules)
    pure ctx

moduleStrategy :: Transform GlobalCheck ModuleCheck
moduleStrategy = reinterpret $ \case
  CreateContext ci ->
    resolveCreateContext ci
  ValidateModule cx md ->
    traverse_ (kCTypeDefinition (md ^. #moduleName) cx) (md ^. #typeDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition modName ctx tyDef -> do
    desiredK <- getSpecifiedKind modName tyDef
    k <- inferTypeKind modName tyDef ctx desiredK
    checkKindConsistency modName tyDef ctx k
  KCClassInstance modName ctx classDef -> do
    _ <- checkClassDefinition modName classDef ctx
    pure ()

runKindCheck :: forall effs {a}. Member Err effs => Eff (KindCheck ': effs) a -> Eff effs a
runKindCheck = interpret $ \case
  InferTypeKind modName tyDef ctx _k ->
    either (handleErr modName tyDef) pure $ I.infer ctx tyDef modName
  CheckKindConsistency modName tyDef ctx k ->
    runReader modName $ resolveKindConsistency tyDef ctx k
  GetSpecifiedKind modName tyDef ->
    fmap snd $ runReader modName $ tyDef2NameAndKind tyDef
  CheckClassDefinition modName classDef ctx ->
    either (handleErr2 modName classDef) pure $ I.runClassDefCheck ctx modName classDef
  where
    handleErr2 :: forall {b}. PC.ModuleName -> PC.ClassDef -> I.InferErr -> Eff effs b
    handleErr2 _ _ _err = error "Throw an error"

    handleErr :: forall {b}. PC.ModuleName -> PC.TyDef -> I.InferErr -> Eff effs b
    handleErr modName td = \case
      I.InferUnboundTermErr ut ->
        case ut of
          QualifiedTyRef qtr -> do
            if qtr ^. qTyRef'moduleName == modName
              then do
                -- We're looking at the local module.
                let localRef = PC.LocalI . fst . withIso ltrISOqtr (\_ f -> f) $ qtr
                let err = PC.UnboundTyRefError td localRef modName
                throwError . PC.CompKindCheckError $ err
              else do
                -- We're looking at a foreign module.
                let foreignRef = PC.ForeignI . withIso ftrISOqtr (\_ f -> f) $ qtr
                throwError . PC.CompKindCheckError $ PC.UnboundTyRefError td foreignRef modName
          TyVar tv ->
            throwError . PC.CompKindCheckError $ PC.UnboundTyVarError td (PC.TyVar tv) modName
          QualifiedTyClassRef _ -> error "NOTE(cstml): FIXME."
      I.InferUnifyTermErr (I.Constraint (k1, k2)) -> do
        err <- PC.IncorrectApplicationError td <$> kind2ProtoKind k1 <*> kind2ProtoKind k2 <*> pure modName
        throwError $ PC.CompKindCheckError err
      I.InferRecursiveSubstitutionErr _ ->
        throwError . PC.CompKindCheckError $ PC.RecursiveKindError td modName
      I.InferImpossibleErr t ->
        throwError $ PC.InternalError t

--------------------------------------------------------------------------------
-- Resolvers
resolveKindConsistency ::
  forall effs.
  Members '[Reader PC.ModuleName, Err] effs =>
  PC.TyDef ->
  Context ->
  Kind ->
  Eff effs Kind
resolveKindConsistency tyDef _ctx inferredKind = do
  (_, k) <- tyDef2NameAndKind tyDef
  guard tyDef k inferredKind
  pure inferredKind
  where
    guard :: PC.TyDef -> Kind -> Kind -> Eff effs ()
    guard t i d
      | i == d = pure ()
      | otherwise = do
          err <- PC.InconsistentTypeError t <$> kind2ProtoKind i <*> kind2ProtoKind d <*> ask
          throwError $ PC.CompKindCheckError err

--------------------------------------------------------------------------------
-- Context Creation

-- | Resolver function for the context creation. There is a guarantee from ProtoCompat that the input is sanitised.
resolveCreateContext :: forall effs. PC.CompilerInput -> Eff effs Context
resolveCreateContext = fmap mconcat . traverse module2Context . toList . view #modules

module2Context :: forall effs. PC.Module -> Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  let classDefinitions = toList $ m ^. #classDefs
  -- Context built from type definitions.
  typeDefCtx <- fmap mconcat . runReader (m ^. #moduleName) $ traverse tyDef2Context typeDefinitions
  -- Context built from class definitions.
  classDefCtx <- fmap mconcat . runReader (m ^. #moduleName) $ traverse classDef2Context classDefinitions
  return $ typeDefCtx <> classDefCtx

--------------------------------------------------------------------------------
-- Type Definition Based Context Building.

-- | Creates a Context entry from one type definition.
tyDef2Context ::
  forall effs.
  Member (Reader PC.ModuleName) effs =>
  PC.TyDef ->
  Eff effs Context
tyDef2Context tyDef = do
  r <- tyDef2NameAndKind tyDef
  pure $ mempty & context .~ uncurry M.singleton r

tyDef2NameAndKind ::
  forall effs.
  Member (Reader PC.ModuleName) effs =>
  PC.TyDef ->
  Eff effs (InfoLess Variable, Kind)
tyDef2NameAndKind tyDef = do
  curModName <- ask
  -- InfoLess Qualified name - the SourceInfo doesn't matter therefore it is defaulted.
  let name =
        QualifiedTyRef
          . withIso ltrISOqtr const
          . (,curModName)
          $ PC.LocalRef (tyDef ^. #tyName) def

      k = tyAbsLHS2Kind (tyDef ^. #tyAbs)

  pure (mkInfoLess name, k)

tyAbsLHS2Kind :: PC.TyAbs -> Kind
tyAbsLHS2Kind tyAbs = foldWithArrowToType $ tyArg2Kind <$> toList (tyAbs ^. #tyArgs)

tyArg2Kind :: PC.TyArg -> Kind
tyArg2Kind = protoKind2Kind . view #argKind

--------------------------------------------------------------------------------
-- Class Definition Based Context Building.

--- | Convert from internal Kind to Proto Kind.
kind2ProtoKind :: forall effs. Member Err effs => Kind -> Eff effs PC.Kind
kind2ProtoKind = \case
  k1 :->: k2 -> fmap PC.Kind $ PC.KindArrow <$> kind2ProtoKind k1 <*> kind2ProtoKind k2
  KType -> pure . PC.Kind . PC.KindRef $ PC.KType
  KVar _ -> pure . PC.Kind . PC.KindRef $ PC.KUnspecified -- this shouldn't happen.
  KConstraint -> pure . PC.Kind . PC.KindRef $ PC.KConstraint

--------------------------------------------------------------------------------
-- Class Definition Based Context Building.

classDef2Context :: forall effs. PC.ClassDef -> Eff effs Context
classDef2Context cDef = do
  let className = mkInfoLess . view #className $ cDef
  let classArg = tyArg2Kind . view #classArgs $ cDef
  pure $ mempty & constraintContext .~ M.singleton className classArg

--------------------------------------------------------------------------------
-- utilities

{- | Folds kinds and appends them to a Kind result type. In essence creates a
 curried function with a Type final kind.

 ghc> foldWithArrowToType []
 Type

 ghc> foldWithArrowToType [Type]
 Type -> Type

 ghc> foldWithArrowToType [Type, (Type -> Type)]
 Type -> (Type -> Type) -> Type
-}
foldWithArrowToType :: [Kind] -> Kind
foldWithArrowToType = foldWithArrowToKind KType

foldWithArrowToKind :: Kind -> [Kind] -> Kind
foldWithArrowToKind = foldr (:->:)
