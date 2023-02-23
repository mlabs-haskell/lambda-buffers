{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck (
  -- * Kindchecking functions.
  check,
  check_,

  -- * Testing functions
  foldWithArrowToType,
) where

import Control.Lens (view, (&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, interpret, reinterpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State (State, evalState)
import Control.Monad.Freer.TH (makeEffect)
import Data.Foldable (Foldable (toList), traverse_)
import Data.Map qualified as M
import LambdaBuffers.Compiler.KindCheck.Context (Context, context)
import LambdaBuffers.Compiler.KindCheck.Inference qualified as I
import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, (:->:)), kind2ProtoKind)
import LambdaBuffers.Compiler.KindCheck.Variable (Variable (ForeignRef, TyVar))
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

-- NOTE(cstml & gnumonik): Lets reach consensus on these - Note(1).
--  KCClassInstance :: Context -> P.InstanceClause -> ModuleCheck ()
--  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  GetSpecifiedKind :: PC.ModuleName -> PC.TyDef -> KindCheck Kind
  InferTypeKind :: PC.ModuleName -> PC.TyDef -> Context -> Kind -> KindCheck Kind
  CheckKindConsistency :: PC.ModuleName -> PC.TyDef -> Context -> Kind -> KindCheck Kind

makeEffect ''KindCheck

--------------------------------------------------------------------------------

-- | The Check effect runner.
runCheck :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck =
  run
    . runError
    . runKindCheck
    . localStrategy
    . moduleStrategy
    . globalStrategy

{- | Run the check - return the validated context or the failure. The main API
 function of the library.
-}
check :: PC.CompilerInput -> PC.CompilerOutput
check = fmap (const PC.CompilerResult) . runCheck . kCheck

-- | Run the check - drop the result if it succeeds - useful for testing.
check_ :: PC.CompilerInput -> Either CompilerErr ()
check_ = void . runCheck . kCheck

--------------------------------------------------------------------------------

{- | A transformation (in the context of the Kind Checker) is a mapping from one
 Effect to another. All effects can fial via the `Err` effect - which is
 essentially the Kind Check failure.
-}
type Transform x y = forall effs {a}. Member Err effs => Eff (x ': effs) a -> Eff (y ': effs) a

-- Transformation strategies
globalStrategy :: Transform Check GlobalCheck
globalStrategy = reinterpret $ \case
  KCheck ci -> do
    ctx <- createContext ci
    void $ validateModule ctx `traverse` (ci ^. #modules)
    pure ctx

moduleStrategy :: Transform GlobalCheck ModuleCheck
moduleStrategy = reinterpret $ \case
  CreateContext ci -> evalState (mempty @(M.Map Variable PC.TyDef)) . resolveCreateContext $ ci
  ValidateModule cx md -> do
    traverse_ (kCTypeDefinition (md ^. #moduleName) cx) (md ^. #typeDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition mname ctx tydef -> do
    desiredK <- getSpecifiedKind mname tydef
    k <- inferTypeKind mname tydef ctx desiredK
    checkKindConsistency mname tydef ctx k

runKindCheck :: forall effs {a}. Member Err effs => Eff (KindCheck ': effs) a -> Eff effs a
runKindCheck = interpret $ \case
  -- TypesFromTyDef modName tydef -> runReader modName (tyDef2Types tydef)
  InferTypeKind modName tyDef ctx k -> either (handleErr modName tyDef) pure $ I.infer ctx tyDef k modName
  CheckKindConsistency modName def ctx k -> runReader modName $ resolveKindConsistency def ctx k
  GetSpecifiedKind modName tyDef -> do
    (_, k) <- tyDef2NameAndKind modName tyDef
    pure k
  where
    handleErr :: forall {b}. PC.ModuleName -> PC.TyDef -> I.InferErr -> Eff effs b
    handleErr modName td = \case
      I.InferUnboundTermErr uA -> do
        case uA of
          ForeignRef fr ->
            if (fr ^. #moduleName) == modName
              then -- We're looking at the local module.
                throwError . PC.CompKindCheckError $ PC.UnboundTyRefError td (PC.LocalI $ fr ^. PC.foreignRef2LocalRef) modName
              else -- We're looking at a foreign module.
                throwError . PC.CompKindCheckError $ PC.UnboundTyRefError td (PC.ForeignI fr) modName
          TyVar tv -> throwError . PC.CompKindCheckError $ PC.UnboundTyVarError td (PC.TyVar tv) modName
      I.InferUnifyTermErr (I.Constraint (k1, k2)) ->
        throwError . PC.CompKindCheckError $ PC.IncorrectApplicationError td (kind2ProtoKind k1) (kind2ProtoKind k2) modName
      I.InferRecursiveSubstitutionErr _ ->
        throwError . PC.CompKindCheckError $ PC.RecursiveKindError td modName
      I.InferImpossibleErr t ->
        throwError . PC.InternalError $ t

-- Resolvers
resolveKindConsistency ::
  forall effs.
  Members '[Reader PC.ModuleName, Err] effs =>
  PC.TyDef ->
  Context ->
  Kind ->
  Eff effs Kind
resolveKindConsistency tydef _ctx inferredKind = do
  modname <- ask @PC.ModuleName
  (_, k) <- tyDef2NameAndKind modname tydef
  guard tydef k inferredKind modname
  pure inferredKind
  where
    guard :: PC.TyDef -> Kind -> Kind -> PC.ModuleName -> Eff effs ()
    guard t i d mn
      | i == d = pure ()
      | otherwise =
          throwError
            . PC.CompKindCheckError
            $ PC.InconsistentTypeError t (kind2ProtoKind i) (kind2ProtoKind d) mn

--------------------------------------------------------------------------------
-- Context Creation

{- | Resolver function for the context creation - it fails if two identical
 declarations are found.
-}
resolveCreateContext ::
  forall effs.
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  PC.CompilerInput ->
  Eff effs Context
resolveCreateContext ci =
  mconcat <$> traverse module2Context (toList $ ci ^. #modules)

module2Context ::
  forall effs.
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  PC.Module ->
  Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  ctxs <-
    runReader (m ^. #moduleName) $
      traverse tyDef2Context typeDefinitions
  pure $ mconcat ctxs

-- | Creates a Context entry from one type definition.
tyDef2Context ::
  forall effs.
  Member (Reader PC.ModuleName) effs =>
  Member Err effs =>
  PC.TyDef ->
  Eff effs Context
tyDef2Context tyDef = do
  curModName <- ask @PC.ModuleName
  r <- tyDef2NameAndKind curModName tyDef
  pure $ mempty & context .~ uncurry M.singleton r

{-
{- | Gets the kind of the variables from the definition and adds them to the
 context.
-}
tyDefArgs2Context :: PC.TyDef -> Eff effs (M.Map Variable Kind)
tyDefArgs2Context tydef = do
  let ds = g <$> M.elems (tydef ^. #tyAbs . #tyArgs)
  pure $ M.fromList ds
  where
    g :: PC.TyArg -> (Variable, Kind)
    g tyarg = (v, k)
      where
        v = TyVar (tyarg ^. #argName)
        k = pKind2Kind (tyarg ^. #argKind)
-}

tyDef2NameAndKind :: forall effs. PC.ModuleName -> PC.TyDef -> Eff effs (Variable, Kind)
tyDef2NameAndKind curModName tyDef = do
  let name = ForeignRef $ view (PC.localRef2ForeignRef curModName) $ PC.LocalRef (tyDef ^. #tyName) (tyDef ^. #sourceInfo)
  let k = tyAbsLHS2Kind (tyDef ^. #tyAbs)
  pure (name, k)

tyAbsLHS2Kind :: PC.TyAbs -> Kind
tyAbsLHS2Kind tyAbs = foldWithArrowToType $ pKind2Kind . (\x -> x ^. #argKind) <$> toList (tyAbs ^. #tyArgs)

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

-- =============================================================================
-- To Kind Conversion functions

pKind2Kind :: PC.Kind -> Kind
pKind2Kind k =
  case k ^. #kind of
    PC.KindRef PC.KType -> KType
    PC.KindArrow l r -> pKind2Kind l :->: pKind2Kind r
    -- NOTE(cstml): What is an undefined Kind type meant to mean?
    _ -> error "Fixme undefined type"
