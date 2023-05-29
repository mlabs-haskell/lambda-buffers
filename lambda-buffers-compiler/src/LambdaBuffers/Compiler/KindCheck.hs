{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck (
  -- * Kind checking functions.
  check,
  runCheck,

  -- * Tested functions
  foldWithArrowToType,
) where

import Control.Lens (Getter, to, view, (&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, interpret, reinterpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.TH (makeEffect)
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList), traverse_)
import Data.Map qualified as M

import LambdaBuffers.Compiler.KindCheck.Derivation (Context, context)
import LambdaBuffers.Compiler.KindCheck.Inference qualified as I
import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, (:->:)), kind2ProtoKind)
import LambdaBuffers.Compiler.KindCheck.Type (Variable (QualifiedTyRef, TyVar))
import LambdaBuffers.ProtoCompat qualified as PC
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess, mkInfoLess)

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
-- Runners

-- | The Check effect runner.
runCheck' :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck' =
  run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

{- | Run the check - return the validated context or the failure. The main API
 function of the library.
-}
check :: PC.CompilerInput -> PC.CompilerOutput
check = fmap (const PC.CompilerResult) . runCheck' . kCheck

-- | Run the check - drop the result if it succeeds - useful for testing.
runCheck :: PC.CompilerInput -> Either CompilerErr ()
runCheck = void . runCheck' . kCheck

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

runKindCheck :: forall effs {a}. Member Err effs => Eff (KindCheck ': effs) a -> Eff effs a
runKindCheck = interpret $ \case
  InferTypeKind modName tyDef ctx k ->
    either (handleErr modName tyDef) pure $ I.infer ctx tyDef k modName
  CheckKindConsistency modName tyDef ctx k ->
    runReader modName $ resolveKindConsistency tyDef ctx k
  GetSpecifiedKind modName tyDef ->
    fmap snd $ runReader modName $ tyDef2NameAndKind tyDef
  where
    handleErr :: forall {b}. PC.ModuleName -> PC.TyDef -> I.InferErr -> Eff effs b
    handleErr modName td = \case
      I.InferUnboundTermErr uA -> do
        case uA of
          QualifiedTyRef fr ->
            if (fr ^. #moduleName) == modName
              then -- We're looking at the local module.

                throwError
                  . PC.CompKindCheckError
                  $ PC.UnboundTyRefError td (PC.LocalI $ fr ^. foreignRef2LocalRef) modName
              else -- We're looking at a foreign module.

                throwError
                  . PC.CompKindCheckError
                  $ PC.UnboundTyRefError td (PC.ForeignI fr) modName
          TyVar tv ->
            throwError
              . PC.CompKindCheckError
              $ PC.UnboundTyVarError td (PC.TyVar tv) modName
      I.InferUnifyTermErr (I.Constraint (k1, k2)) ->
        throwError
          . PC.CompKindCheckError
          $ PC.IncorrectApplicationError td (kind2ProtoKind k1) (kind2ProtoKind k2) modName
      I.InferRecursiveSubstitutionErr _ ->
        throwError
          . PC.CompKindCheckError
          $ PC.RecursiveKindError td modName
      I.InferImpossibleErr t ->
        throwError $
          PC.InternalError t

    foreignRef2LocalRef :: Getter PC.ForeignRef PC.LocalRef
    foreignRef2LocalRef =
      to
        ( \fr ->
            PC.LocalRef
              { tyName = fr ^. #tyName
              , sourceInfo = fr ^. #sourceInfo
              }
        )

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
          mn <- ask
          throwError
            . PC.CompKindCheckError
            $ PC.InconsistentTypeError t (kind2ProtoKind i) (kind2ProtoKind d) mn

--------------------------------------------------------------------------------
-- Context Creation

-- | Resolver function for the context creation. There is a guarantee from ProtoCompat that the input is sanitised.
resolveCreateContext :: forall effs. PC.CompilerInput -> Eff effs Context
resolveCreateContext ci =
  mconcat <$> traverse module2Context (toList $ ci ^. #modules)

module2Context :: forall effs. PC.Module -> Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  ctxs <- runReader (m ^. #moduleName) $ traverse tyDef2Context typeDefinitions
  pure $ mconcat ctxs

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

  -- InfoLess name - the SourceInfo doesn't matter therefore it is defaulted.
  let name =
        QualifiedTyRef
          . view (PC.localRef2ForeignRef curModName)
          $ PC.LocalRef (tyDef ^. #tyName) def

      k = tyAbsLHS2Kind (tyDef ^. #tyAbs)

  pure (mkInfoLess name, k)

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

--------------------------------------------------------------------------------
-- To Kind Conversion functions

pKind2Kind :: PC.Kind -> Kind
pKind2Kind k =
  case k ^. #kind of
    PC.KindRef PC.KType -> KType
    PC.KindArrow l r -> pKind2Kind l :->: pKind2Kind r
    PC.KindRef PC.KUnspecified -> KType -- (for now) defaulting unspecified kinds to Type
