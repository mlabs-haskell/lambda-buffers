{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Note: At the moment the Kind Checker disregards multiple Modules for
simplicity of testing and developing. This will be changed ASAP. :fixme:
-}
module LambdaBuffers.Compiler.KindCheck (
  KindCheckFailure (..),
  check,
  foldWithSum,

  -- * Testing Utils

  -- * Utilities -- exported for testing
  foldWithArrow,
  foldWithProduct,
) where

import Control.Exception (Exception)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.TH (makeEffect)
import Data.Text (Text, intercalate)
import LambdaBuffers.Compiler.KindCheck.Inference (
  Context,
  InferErr,
  Kind (Type, (:->:)),
  Type (Var),
  context,
  infer,
 )

import Control.Monad (void)
import Control.Monad.Freer (Eff, Members, reinterpret, run)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import LambdaBuffers.Common.ProtoCompat qualified as P
import LambdaBuffers.Compiler.KindCheck.Type (Type (App))

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)), uncons, (<|))
import Data.Map qualified as M

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
data KindCheckFailure
  = CheckFailure String
  | LookupVarFailure Text
  | LookupRefFailure P.TyRef
  | AppWrongArgKind Kind Kind -- Expected Kind got Kind
  | AppToManyArgs Int
  | InvalidProto Text
  | AppNoArgs -- No args
  | InvalidType
  | InferenceFailed P.TyDef InferErr
  deriving stock (Show, Eq)

instance Exception KindCheckFailure

type Err = Error KindCheckFailure

type ModuleName = Text

-- | Main interface to the Kind Checker.
data Check a where
  KCheck :: P.CompilerInput -> Check ()

makeEffect ''Check

-- | Interactions that happen at the level of the Global Checker.
data GlobalCheck a where
  CreateContext :: P.CompilerInput -> GlobalCheck Context
  ValidateModule :: Context -> P.Module -> GlobalCheck ()

makeEffect ''GlobalCheck

-- | Interactions that happen at the level of the
data ModuleCheck a where -- Module
  KCTypeDefinition :: ModuleName -> Context -> P.TyDef -> ModuleCheck Kind
  KCClassInstance :: Context -> P.InstanceClause -> ModuleCheck ()
  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  KindFromTyDef :: ModuleName -> P.TyDef -> KindCheck Type
  InferTypeKind :: Context -> Type -> KindCheck Kind
  CheckKindConsistency :: P.TyDef -> Context -> Kind -> KindCheck Kind
makeEffect ''KindCheck

type Transform x y = forall effs {a}. Eff (x ': effs) a -> Eff (y ': effs) a

-- Transformation strategies
globalStrategy :: Transform Check GlobalCheck
globalStrategy = reinterpret $ \case
  KCheck ci -> do
    ctx <- createContext ci
    void $ validateModule ctx `traverse` (ci ^. #modules)

moduleStrategy :: Transform GlobalCheck ModuleCheck
moduleStrategy = reinterpret $ \case
  CreateContext ci -> resolveCreateContext ci
  ValidateModule cx md -> do
    traverse_ (kCTypeDefinition (module2ModuleName md) cx) (md ^. #typeDefs)
    traverse_ (kCClassInstance cx) (md ^. #instances)
    traverse_ (kCClass cx) (md ^. #classDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition mname ctx tydef -> do
    kindFromTyDef mname tydef >>= inferTypeKind ctx >>= checkKindConsistency tydef ctx
  KCClassInstance ctx instClause -> error "FIXME"
  KCClass ctx classDef -> error "Fixme"

runKindCheck :: Eff '[KindCheck] a -> Eff '[Err] a
runKindCheck = reinterpret $ \case
  KindFromTyDef moduleName tydef -> runReader moduleName (tyDef2Type tydef)
  InferTypeKind ctx ty -> either (\_ -> throwError InvalidType) pure $ infer ctx ty
  CheckKindConsistency def ctx k -> resolveKindConsistency def ctx k

runCheck :: Eff (Check ': '[]) a -> Either KindCheckFailure a
runCheck = run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

check :: P.CompilerInput -> Either KindCheckFailure ()
check = runCheck . kCheck

-- Resolvers

resolveKindConsistency tydef ctx k = undefined

resolveCreateContext :: forall effs. P.CompilerInput -> Eff effs Context
resolveCreateContext ci = mconcat <$> traverse module2Context (ci ^. #modules)

module2Context :: forall effs. P.Module -> Eff effs Context
module2Context m = mconcat <$> traverse (tyDef2Context (flattenModuleName (m ^. #moduleName))) (P.typeDefs m)

flattenModuleName :: P.ModuleName -> Text
flattenModuleName mName = intercalate "." $ (\p -> p ^. #name) <$> mName ^. #parts

tyDef2Context :: forall effs. ModuleName -> P.TyDef -> Eff effs Context
tyDef2Context curModName tyDef = do
  let name = curModName <> "." <> (tyDef ^. #tyName . #name) -- name is qualified
  let ty = tyAbsLHS2Type (tyDef ^. #tyAbs)
  pure $ mempty & context .~ M.singleton name ty

tyAbsLHS2Type :: P.TyAbs -> Kind
tyAbsLHS2Type tyAbs = foldWithArrow $ pKind2Type . (\x -> x ^. #argKind) <$> (tyAbs ^. #tyArgs)

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

-- =============================================================================
-- X To Canonical type conversion functions.

-- | TyDef to Kind Canonical representation.
tyDef2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.TyDef ->
  Eff eff Type
tyDef2Type tyde = tyAbsRHS2Type (tyde ^. #tyAbs)

tyAbsRHS2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.TyAbs ->
  Eff eff Type
tyAbsRHS2Type tyab = tyBody2Type (tyab ^. #tyBody)

tyBody2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.TyBody ->
  Eff eff Type
tyBody2Type = \case
  P.OpaqueI _ -> pure $ Var "Opaque"
  P.SumI s -> sum2Type s

sum2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Sum ->
  Eff eff Type
sum2Type su = foldWithSum <$> traverse constructor2Type (su ^. #constructors)

constructor2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Constructor ->
  Eff eff Type
constructor2Type co = product2Type (co ^. #product)

product2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Product ->
  Eff eff Type
product2Type = \case
  P.RecordI r -> record2Type r
  P.TupleI t -> tuple2Type t

record2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Record ->
  Eff eff Type
record2Type r = foldWithProduct <$> traverse field2Type (r ^. #fields)

tuple2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Tuple ->
  Eff eff Type
tuple2Type tu = do
  tup <- traverse ty2Type $ tu ^. #fields
  case tup of
    [] -> pure $ Var "𝟙"
    x : xs -> pure . foldWithProduct $ x :| xs

field2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Field ->
  Eff eff Type
field2Type f = ty2Type (f ^. #fieldTy)

ty2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.Ty ->
  Eff eff Type
ty2Type = \case
  P.TyVarI tytv -> tyVar2Type tytv
  P.TyAppI tyap -> tyApp2Type tyap
  P.TyRefI tyre -> tyRef2Type tyre

tyVar2Type ::
  forall eff.
  Members '[] eff =>
  P.TyVar ->
  Eff eff Type
tyVar2Type tv = pure . Var $ (tv ^. #varName . #name)

tyApp2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.TyApp ->
  Eff eff Type
tyApp2Type ta = do
  fn <- ty2Type (ta ^. #tyFunc)
  args <- traverse ty2Type (ta ^. #tyArgs)
  pure $ foldWithApp (fn <| args)

tyRef2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.TyRef ->
  Eff eff Type
tyRef2Type = \case
  P.LocalI lref -> localTyRef2Type lref
  P.ForeignI fref -> foreignTyRef2Type fref

localTyRef2Type ::
  forall eff.
  Members '[Reader ModuleName, Err] eff =>
  P.LocalRef ->
  Eff eff Type
localTyRef2Type ltr = do
  moduleName <- ask
  pure $ Var $ moduleName <> "." <> (ltr ^. #tyName . #name)

foreignTyRef2Type ::
  forall eff.
  Members '[] eff =>
  P.ForeignRef ->
  Eff eff Type
foreignTyRef2Type ftr = do
  let moduleName = flattenModuleName (ftr ^. #moduleName)
  pure $ Var $ moduleName <> "." <> (ftr ^. #tyName . #name)

-- Utilities

foldWithApp :: NonEmpty Type -> Type
foldWithApp = foldWithBinaryOp App

foldWithProduct :: NonEmpty Type -> Type
foldWithProduct = foldWithBinaryOp $ App . App (Var "Π")

foldWithSum :: NonEmpty Type -> Type
foldWithSum = foldWithBinaryOp $ App . App (Var "Σ")

-- | Generic way of folding.
foldWithBinaryOp :: (Type -> Type -> Type) -> NonEmpty Type -> Type
foldWithBinaryOp op ne = case uncons ne of
  (x, Nothing) -> x
  (x, Just xs) -> op x $ foldWithBinaryOp op xs

module2ModuleName :: P.Module -> ModuleName
module2ModuleName = flattenModuleName . (^. #moduleName)
