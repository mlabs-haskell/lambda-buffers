{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck (
  -- * Kindchecking functions.
  check,
  check_,

  -- * Testing Utils.
  foldWithSum,
  foldWithArrow,
  foldWithProduct,
) where

import Control.Lens (view, (&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, interpret, reinterpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State (State, evalState, modify)
import Control.Monad.Freer.TH (makeEffect)
import Data.Foldable (Foldable (foldl', toList), traverse_)
import Data.Map qualified as M
import Data.Text (Text, intercalate)
import LambdaBuffers.Compiler.KindCheck.Context (Context)
import LambdaBuffers.Compiler.KindCheck.Inference (
  InferErr (
    InferImpossibleErr,
    InferRecursiveSubstitutionErr,
    InferUnboundTermErr,
    InferUnifyTermErr
  ),
  Kind (Type, (:->:)),
  Type (Abs, Var),
  context,
  infer,
 )
import LambdaBuffers.Compiler.KindCheck.Inference qualified as I
import LambdaBuffers.Compiler.KindCheck.Kind (kind2ProtoKind)
import LambdaBuffers.Compiler.KindCheck.Type (Type (App), tyEither, tyOpaque, tyProd, tyUnit, tyVoid)
import LambdaBuffers.Compiler.KindCheck.Variable (Variable (ForeignRef, LocalRef))
import LambdaBuffers.Compiler.ProtoCompat ()
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
type CompilerErr = PC.CompilerError

type Err = Error CompilerErr

type ModName = [Text]

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
  KCTypeDefinition :: ModName -> Context -> PC.TyDef -> ModuleCheck Kind
  KCClassInstance :: Context -> PC.InstanceClause -> ModuleCheck ()
  KCClass :: Context -> PC.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  KindFromTyDef :: ModName -> PC.TyDef -> KindCheck Type
  InferTypeKind :: ModName -> PC.TyDef -> Context -> Type -> KindCheck Kind
  CheckKindConsistency :: ModName -> PC.TyDef -> Context -> Kind -> KindCheck Kind

-- FIXME(cstml): Add check for Context Consistency
-- TyDefToTypes :: ModName -> PC.TyDef -> KindCheck [Type]
makeEffect ''KindCheck

--------------------------------------------------------------------------------

-- | The Check effect runner.
runCheck :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck = run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

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
    traverse_ (kCTypeDefinition (module2ModuleName md) cx) (md ^. #typeDefs)
    traverse_ (kCClassInstance cx) (md ^. #instances)
    traverse_ (kCClass cx) (md ^. #classDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition mname ctx tydef -> do
    kindFromTyDef mname tydef >>= inferTypeKind mname tydef ctx >>= checkKindConsistency mname tydef ctx
  KCClassInstance _ctx _instClause -> pure () -- FIXME(cstml)
  KCClass _ctx _classDef -> pure () --  FIXME(cstml)

runKindCheck :: forall effs {a}. Member Err effs => Eff (KindCheck ': effs) a -> Eff effs a
runKindCheck = interpret $ \case
  KindFromTyDef moduleName tydef -> runReader moduleName (tyDef2Type tydef)
  -- TyDefToTypes moduleName tydef -> runReader moduleName (tyDef2Types tydef)
  InferTypeKind _modName tyDef ctx ty -> either (handleErr tyDef) pure $ infer ctx ty
  CheckKindConsistency mname def ctx k -> runReader mname $ resolveKindConsistency def ctx k
  where
    handleErr :: forall {b}. PC.TyDef -> InferErr -> Eff effs b
    handleErr td = \case
      InferUnboundTermErr uA ->
        throwError . PC.CompKindCheckError $ PC.UnboundTermError (tyDef2TyName td) (var2VarName uA)
      InferUnifyTermErr (I.Constraint (k1, k2)) ->
        throwError . PC.CompKindCheckError $ PC.IncorrectApplicationError (tyDef2TyName td) (kind2ProtoKind k1) (kind2ProtoKind k2)
      InferRecursiveSubstitutionErr _ ->
        throwError . PC.CompKindCheckError $ PC.RecursiveKindError $ tyDef2TyName td
      InferImpossibleErr t ->
        throwError . PC.InternalError $ t

    var2VarName = \case
      LocalRef n -> PC.VarName n emptySourceInfo
      ForeignRef m s -> PC.VarName (intercalate "." m <> s) emptySourceInfo

    emptySourceInfo = PC.SourceInfo mempty emptySourcePosition emptySourcePosition

    emptySourcePosition = PC.SourcePosition 0 0

-- Resolvers

resolveKindConsistency ::
  forall effs.
  Members '[Reader ModName, Err] effs =>
  PC.TyDef ->
  Context ->
  Kind ->
  Eff effs Kind
resolveKindConsistency tydef _ctx inferredKind = do
  mName <- ask @ModName
  let tyName = tyDef2TyName tydef
  (_, k) <- tyDef2NameAndKind mName tydef
  guard tyName k inferredKind
  pure inferredKind
  where
    guard :: PC.TyName -> Kind -> Kind -> Eff effs ()
    guard n i d
      | i == d = pure ()
      | otherwise =
          throwError . PC.CompKindCheckError $
            PC.InconsistentTypeError n (kind2ProtoKind i) (kind2ProtoKind d)

tyDef2TyName :: PC.TyDef -> PC.TyName
tyDef2TyName (PC.TyDef n _ _) = n

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
resolveCreateContext ci = do
  ctxs <- traverse module2Context (toList $ ci ^. #modules)
  pure $ mconcat ctxs

module2Context ::
  forall effs.
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  PC.Module ->
  Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  ctxs <- runReader (m ^. #moduleName) $ do
    traverse (tyDef2Context (moduleName2ModName (m ^. #moduleName))) typeDefinitions
  pure $ mconcat ctxs

-- | Creates a Context entry from one type definition.
tyDef2Context ::
  forall effs.
  Member (Reader PC.ModuleName) effs =>
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  ModName ->
  PC.TyDef ->
  Eff effs Context
tyDef2Context curModName tyDef = do
  r@(v, _) <- tyDef2NameAndKind curModName tyDef
  associateName v tyDef
  pure $ mempty & context .~ uncurry M.singleton r
  where
    -- Ads the name to our map - we can use its SourceLocation in the case of a
    -- double use. If it's already in our map - that means we've double declared it.
    associateName :: Variable -> PC.TyDef -> Eff effs ()
    associateName v curTyDef = modify (M.insert v curTyDef)

{- | Converts the Proto Module name to a local modname - dropping the
 information.
-}
moduleName2ModName :: PC.ModuleName -> ModName
moduleName2ModName mName = (\p -> p ^. #name) <$> mName ^. #parts

tyDef2NameAndKind :: forall effs. ModName -> PC.TyDef -> Eff effs (Variable, Kind)
tyDef2NameAndKind curModName tyDef = do
  -- all names are qualified
  let name = ForeignRef curModName (tyDef ^. #tyName . #name)
  let k = tyAbsLHS2Kind (tyDef ^. #tyAbs)
  pure (name, k)

tyAbsLHS2Kind :: PC.TyAbs -> Kind
tyAbsLHS2Kind tyAbs = foldWithArrow $ pKind2Type . (\x -> x ^. #argKind) <$> toList (tyAbs ^. #tyArgs)

foldWithArrow :: [Kind] -> Kind
foldWithArrow = foldl (:->:) Type

-- ================================================================================
-- To Kind Conversion functions

pKind2Type :: PC.Kind -> Kind
pKind2Type k =
  case k ^. #kind of
    PC.KindRef PC.KType -> Type
    PC.KindArrow l r -> pKind2Type l :->: pKind2Type r
    -- FIXME(cstml) what is an undefined type meant to mean?
    _ -> error "Fixme undefined type"

{- | TyDef to Kind Canonical representation.
 TODO(@cstml): Move this close to KindCheck/Type.hs (even just there).
-}
tyDef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyDef ->
  Eff eff Type
tyDef2Type tyde = tyAbsLHS2Type (tyde ^. #tyAbs) <*> tyAbsRHS2Type (tyde ^. #tyAbs)

tyAbsLHS2Type ::
  forall eff.
  PC.TyAbs ->
  Eff eff (Type -> Type)
tyAbsLHS2Type tyab = tyArgs2Type (toList $ tyab ^. #tyArgs)

tyArgs2Type ::
  forall eff.
  [PC.TyArg] ->
  Eff eff (Type -> Type)
tyArgs2Type = \case
  [] -> pure id
  x : xs -> do
    f <- tyArgs2Type xs
    pure $ \c -> Abs (tyArg2Var x) (f c)

tyArg2Var :: PC.TyArg -> Variable
tyArg2Var = LocalRef . view (#argName . #name)

tyAbsRHS2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyAbs ->
  Eff eff Type
tyAbsRHS2Type tyab = tyBody2Type (tyab ^. #tyBody)

tyBody2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyBody ->
  Eff eff Type
tyBody2Type = \case
  PC.OpaqueI _ -> pure $ Var tyOpaque
  PC.SumI s -> sum2Type s

sum2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Sum ->
  Eff eff Type
sum2Type su = foldWithSum <$> traverse constructor2Type (toList $ su ^. #constructors)

constructor2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Constructor ->
  Eff eff Type
constructor2Type co = product2Type (co ^. #product)

product2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Product ->
  Eff eff Type
product2Type = \case
  PC.RecordI r -> record2Type r
  PC.TupleI t -> tuple2Type t

record2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Record ->
  Eff eff Type
record2Type r = foldWithProduct <$> traverse field2Type (toList $ r ^. #fields)

tuple2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Tuple ->
  Eff eff Type
tuple2Type tu = do
  tup <- traverse ty2Type $ tu ^. #fields
  pure . foldWithProduct $ tup

field2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Field ->
  Eff eff Type
field2Type f = ty2Type (f ^. #fieldTy)

ty2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Ty ->
  Eff eff Type
ty2Type = \case
  PC.TyVarI tytv -> tyVar2Type tytv
  PC.TyAppI tyap -> tyApp2Type tyap
  PC.TyRefI tyre -> tyRef2Type tyre

tyVar2Type ::
  forall eff.
  PC.TyVar ->
  Eff eff Type
tyVar2Type tv = pure . Var . LocalRef $ (tv ^. #varName . #name)

tyApp2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyApp ->
  Eff eff Type
tyApp2Type ta = do
  fn <- ty2Type (ta ^. #tyFunc)
  args <- traverse ty2Type (toList $ ta ^. #tyArgs)
  pure $ foldWithApp fn args

tyRef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyRef ->
  Eff eff Type
tyRef2Type = \case
  PC.LocalI lref -> localTyRef2Type lref
  PC.ForeignI fref -> foreignTyRef2Type fref

localTyRef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.LocalRef ->
  Eff eff Type
localTyRef2Type ltr = do
  moduleName <- ask
  pure . Var $ ForeignRef moduleName (ltr ^. #tyName . #name)

foreignTyRef2Type ::
  forall eff.
  PC.ForeignRef ->
  Eff eff Type
foreignTyRef2Type ftr = do
  let moduleName = moduleName2ModName (ftr ^. #moduleName)
  pure $ Var $ ForeignRef moduleName (ftr ^. #tyName . #name)

-- =============================================================================
-- X To Canonical type conversion functions.
{-
-- | TyDef to Kind Canonical representation - sums not folded - therefore we get constructor granularity. Might use in a different implementation for more granular errors.
tyDef2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyDef ->
  Eff eff [Type]
tyDef2Types tyde = do
  f <- tyAbsLHS2Type (tyde ^. #tyAbs) -- abstraction
  cs <- tyAbsRHS2Types (tyde ^. #tyAbs) --
  pure $ f <$> cs

tyAbsRHS2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyAbs ->
  Eff eff [Type]
tyAbsRHS2Types tyab = tyBody2Types (tyab ^. #tyBody)

tyBody2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyBody ->
  Eff eff [Type]
tyBody2Types = \case
  PC.OpaqueI _ -> pure [Var $ LocalRef "Opaque"]
  PC.SumI s -> sum2Types s

sum2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Sum ->
  Eff eff [Type]
sum2Types su = NonEmpty.toList <$> traverse constructor2Type (su ^. #constructors)
-}
--------------------------------------------------------------------------------
-- Utilities

foldWithApp :: Type -> [Type] -> Type
foldWithApp = foldl' App

foldWithProduct :: [Type] -> Type
foldWithProduct = foldl' (App . App (Var tyProd)) (Var tyUnit)

foldWithSum :: [Type] -> Type
foldWithSum = foldl' (App . App (Var tyEither)) (Var tyVoid)

module2ModuleName :: PC.Module -> ModName
module2ModuleName = moduleName2ModName . (^. #moduleName)
