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
import LambdaBuffers.Compiler.KindCheck.Type (Type (App), tyEither, tyOpaque, tyProd, tyUnit, tyVoid)
import LambdaBuffers.Compiler.KindCheck.Variable (Variable (ForeignRef, LocalRef))
import LambdaBuffers.Compiler.ProtoCompat (kind2ProtoKind)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PT

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
type CompilerErr = P.CompilerError

type Err = Error CompilerErr

type ModName = [Text]

-- | Main interface to the Kind Checker.
data Check a where
  KCheck :: P.CompilerInput -> Check Context

makeEffect ''Check

-- | Interactions that happen at the level of the Global Checker.
data GlobalCheck a where
  CreateContext :: P.CompilerInput -> GlobalCheck Context
  ValidateModule :: Context -> P.Module -> GlobalCheck ()

makeEffect ''GlobalCheck

-- | Interactions that happen at the level of the
data ModuleCheck a where -- Module
  KCTypeDefinition :: ModName -> Context -> P.TyDef -> ModuleCheck Kind
  KCClassInstance :: Context -> P.InstanceClause -> ModuleCheck ()
  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  KindFromTyDef :: ModName -> P.TyDef -> KindCheck Type
  InferTypeKind :: ModName -> P.TyDef -> Context -> Type -> KindCheck Kind
  CheckKindConsistency :: ModName -> P.TyDef -> Context -> Kind -> KindCheck Kind

-- FIXME(cstml) add check for Context Consistency
-- TyDefToTypes :: ModName -> P.TyDef -> KindCheck [Type]
makeEffect ''KindCheck

--------------------------------------------------------------------------------

-- | The Check effect runner.
runCheck :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck = run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

{- | Run the check - return the validated context or the failure. The main API
 function of the library.
-}
check :: P.CompilerInput -> PT.CompilerOutput
check = fmap (const PT.CompilerResult) . runCheck . kCheck

-- | Run the check - drop the result if it succeeds - useful for testing.
check_ :: P.CompilerInput -> Either CompilerErr ()
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
  CreateContext ci -> evalState (mempty @(M.Map Variable P.TyDef)) . resolveCreateContext $ ci
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
    handleErr :: forall {b}. P.TyDef -> InferErr -> Eff effs b
    handleErr td = \case
      InferUnboundTermErr uA ->
        throwError . P.CompKindCheckError $ P.UnboundTermError (tyDef2TyName td) (var2VarName uA)
      InferUnifyTermErr (I.Constraint (k1, k2)) ->
        throwError . P.CompKindCheckError $ P.IncorrectApplicationError (tyDef2TyName td) (kind2ProtoKind k1) (kind2ProtoKind k2)
      InferRecursiveSubstitutionErr _ ->
        throwError . P.CompKindCheckError $ P.RecursiveKindError $ tyDef2TyName td
      InferImpossibleErr t ->
        throwError . P.InternalError $ t

    var2VarName = \case
      LocalRef n -> P.VarName n emptySourceInfo
      ForeignRef m s -> P.VarName (intercalate "." m <> s) emptySourceInfo

    emptySourceInfo = P.SourceInfo mempty emptySourcePosition emptySourcePosition

    emptySourcePosition = P.SourcePosition 0 0

-- Resolvers

resolveKindConsistency ::
  forall effs.
  Members '[Reader ModName, Err] effs =>
  P.TyDef ->
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
    guard :: P.TyName -> Kind -> Kind -> Eff effs ()
    guard n i d
      | i == d = pure ()
      | otherwise =
          throwError . P.CompKindCheckError $
            P.InconsistentTypeError n (kind2ProtoKind i) (kind2ProtoKind d)

tyDef2TyName :: P.TyDef -> P.TyName
tyDef2TyName (P.TyDef n _ _) = n

--------------------------------------------------------------------------------
-- Context Creation

{- | Resolver function for the context creation - it fails if two identical
 declarations are found.
-}
resolveCreateContext ::
  forall effs.
  Member (State (M.Map Variable P.TyDef)) effs =>
  Member Err effs =>
  P.CompilerInput ->
  Eff effs Context
resolveCreateContext ci = do
  ctxs <- traverse module2Context (toList $ ci ^. #modules)
  pure $ mconcat ctxs

module2Context ::
  forall effs.
  Member (State (M.Map Variable P.TyDef)) effs =>
  Member Err effs =>
  P.Module ->
  Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  ctxs <- runReader (m ^. #moduleName) $ do
    traverse (tyDef2Context (moduleName2ModName (m ^. #moduleName))) typeDefinitions
  pure $ mconcat ctxs

-- | Creates a Context entry from one type definition.
tyDef2Context ::
  forall effs.
  Member (Reader P.ModuleName) effs =>
  Member (State (M.Map Variable P.TyDef)) effs =>
  Member Err effs =>
  ModName ->
  P.TyDef ->
  Eff effs Context
tyDef2Context curModName tyDef = do
  r@(v, _) <- tyDef2NameAndKind curModName tyDef
  associateName v tyDef
  pure $ mempty & context .~ uncurry M.singleton r
  where
    -- Ads the name to our map - we can use its SourceLocation in the case of a
    -- double use. If it's already in our map - that means we've double declared it.
    associateName :: Variable -> P.TyDef -> Eff effs ()
    associateName v curTyDef = modify (M.insert v curTyDef)

{- | Converts the Proto Module name to a local modname - dropping the
 information.
-}
moduleName2ModName :: P.ModuleName -> ModName
moduleName2ModName mName = (\p -> p ^. #name) <$> mName ^. #parts

tyDef2NameAndKind :: forall effs. ModName -> P.TyDef -> Eff effs (Variable, Kind)
tyDef2NameAndKind curModName tyDef = do
  -- all names are qualified
  let name = ForeignRef curModName (tyDef ^. #tyName . #name)
  let k = tyAbsLHS2Kind (tyDef ^. #tyAbs)
  pure (name, k)

tyAbsLHS2Kind :: P.TyAbs -> Kind
tyAbsLHS2Kind tyAbs = foldWithArrow $ pKind2Type . (\x -> x ^. #argKind) <$> toList (tyAbs ^. #tyArgs)

foldWithArrow :: [Kind] -> Kind
foldWithArrow = foldl (:->:) Type

-- ================================================================================
-- To Kind Conversion functions

pKind2Type :: P.Kind -> Kind
pKind2Type k =
  case k ^. #kind of
    P.KindRef P.KType -> Type
    P.KindArrow l r -> pKind2Type l :->: pKind2Type r
    -- FIXME(cstml) what is an undefined type meant to mean?
    _ -> error "Fixme undefined type"

{- | TyDef to Kind Canonical representation.
 TODO(@cstml): Move this close to KindCheck/Type.hs (even just there).
-}
tyDef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyDef ->
  Eff eff Type
tyDef2Type tyde = tyAbsLHS2Type (tyde ^. #tyAbs) <*> tyAbsRHS2Type (tyde ^. #tyAbs)

tyAbsLHS2Type ::
  forall eff.
  P.TyAbs ->
  Eff eff (Type -> Type)
tyAbsLHS2Type tyab = tyArgs2Type (toList $ tyab ^. #tyArgs)

tyArgs2Type ::
  forall eff.
  [P.TyArg] ->
  Eff eff (Type -> Type)
tyArgs2Type = \case
  [] -> pure id
  x : xs -> do
    f <- tyArgs2Type xs
    pure $ \c -> Abs (tyArg2Var x) (f c)

tyArg2Var :: P.TyArg -> Variable
tyArg2Var = LocalRef . view (#argName . #name)

tyAbsRHS2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyAbs ->
  Eff eff Type
tyAbsRHS2Type tyab = tyBody2Type (tyab ^. #tyBody)

tyBody2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyBody ->
  Eff eff Type
tyBody2Type = \case
  P.OpaqueI _ -> pure $ Var tyOpaque
  P.SumI s -> sum2Type s

sum2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Sum ->
  Eff eff Type
sum2Type su = foldWithSum <$> traverse constructor2Type (toList $ su ^. #constructors)

constructor2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Constructor ->
  Eff eff Type
constructor2Type co = product2Type (co ^. #product)

product2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Product ->
  Eff eff Type
product2Type = \case
  P.RecordI r -> record2Type r
  P.TupleI t -> tuple2Type t

record2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Record ->
  Eff eff Type
record2Type r = foldWithProduct <$> traverse field2Type (toList $ r ^. #fields)

tuple2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Tuple ->
  Eff eff Type
tuple2Type tu = do
  tup <- traverse ty2Type $ tu ^. #fields
  pure . foldWithProduct $ tup

field2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Field ->
  Eff eff Type
field2Type f = ty2Type (f ^. #fieldTy)

ty2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Ty ->
  Eff eff Type
ty2Type = \case
  P.TyVarI tytv -> tyVar2Type tytv
  P.TyAppI tyap -> tyApp2Type tyap
  P.TyRefI tyre -> tyRef2Type tyre

tyVar2Type ::
  forall eff.
  P.TyVar ->
  Eff eff Type
tyVar2Type tv = pure . Var . LocalRef $ (tv ^. #varName . #name)

tyApp2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyApp ->
  Eff eff Type
tyApp2Type ta = do
  fn <- ty2Type (ta ^. #tyFunc)
  args <- traverse ty2Type (toList $ ta ^. #tyArgs)
  pure $ foldWithApp fn args

tyRef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyRef ->
  Eff eff Type
tyRef2Type = \case
  P.LocalI lref -> localTyRef2Type lref
  P.ForeignI fref -> foreignTyRef2Type fref

localTyRef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.LocalRef ->
  Eff eff Type
localTyRef2Type ltr = do
  moduleName <- ask
  pure . Var $ ForeignRef moduleName (ltr ^. #tyName . #name)

foreignTyRef2Type ::
  forall eff.
  P.ForeignRef ->
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
  P.TyDef ->
  Eff eff [Type]
tyDef2Types tyde = do
  f <- tyAbsLHS2Type (tyde ^. #tyAbs) -- abstraction
  cs <- tyAbsRHS2Types (tyde ^. #tyAbs) --
  pure $ f <$> cs

tyAbsRHS2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyAbs ->
  Eff eff [Type]
tyAbsRHS2Types tyab = tyBody2Types (tyab ^. #tyBody)

tyBody2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.TyBody ->
  Eff eff [Type]
tyBody2Types = \case
  P.OpaqueI _ -> pure [Var $ LocalRef "Opaque"]
  P.SumI s -> sum2Types s

sum2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  P.Sum ->
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

module2ModuleName :: P.Module -> ModName
module2ModuleName = moduleName2ModName . (^. #moduleName)
