{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Note: At the moment the Kind Checker disregards multiple Modules for
simplicity of testing and developing. This will be changed ASAP. :fixme:
-}
module LambdaBuffers.Compiler.KindCheck (
  KindCheckFailure (..),
  runKindCheck,

  -- * Testing Utils
  kindCheckType,
  runKindCheckEff,
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

import Control.Monad
import Control.Monad.Freer
import LambdaBuffers.Common.ProtoCompat qualified as P
import LambdaBuffers.Compiler.KindCheck.Type

import Data.Foldable
import Data.List.NonEmpty
import Data.Map qualified as M
import Data.Text qualified as T

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

-- | Main interface to the Kind Checker.
data Check a where
  KindCheck :: P.CompilerInput -> Check ()

makeEffect ''Check

-- | Interactions that happen at the level of the Global Checker.
data GlobalCheck a where
  CreateContext :: P.CompilerInput -> GlobalCheck Context
  ValidateModule :: Context -> P.Module -> GlobalCheck ()

makeEffect ''GlobalCheck

-- | Interactions that happen at the level of the
data ModuleCheck a where -- Module
  KCTypeDefinition :: Context -> P.TyDef -> ModuleCheck Kind
  KCClassInstance :: Context -> P.InstanceClause -> ModuleCheck ()
  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  KindFromTyDef :: P.TyDef -> KindCheck Type
  InferTypeKind :: Context -> Type -> KindCheck Kind
  CheckKindConsistency :: P.TyDef -> Context -> Kind -> KindCheck Kind
makeEffect ''KindCheck

type Transform x y = forall effs {a}. Eff (x ': effs) a -> Eff (y ': effs) a

-- Transformation strategies

globalStrategy :: Transform Check GlobalCheck
globalStrategy = reinterpret $ \case
  KindCheck ci -> do
    ctx <- createContext ci
    void $ validateModule ctx `traverse` (ci ^. #modules)

moduleStrategy :: Transform GlobalCheck ModuleCheck
moduleStrategy = reinterpret $ \case
  CreateContext ci -> resolveCreateContext ci
  ValidateModule cx md -> do
    traverse_ (kCTypeDefinition cx) (md ^. #typeDefs)
    traverse_ (kCClassInstance cx) (md ^. #instances)
    traverse_ (kCClass cx) (md ^. #classDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition ctx tydef -> kindFromTyDef tydef >>= inferTypeKind ctx >>= checkKindConsistency tydef ctx
  KCClassInstance ctx instClause -> error "FIXME"
  KCClass ctx classDef -> error "Fixme"

runKindCheck :: Eff '[KindCheck] a -> Eff '[Err] a
runKindCheck = reinterpret $ \case
  KindFromTyDef tydef -> tyDef2Type tydef
  InferTypeKind ctx ty -> either (\_ -> throwError InvalidType) pure $ infer ctx ty
  CheckKindConsistency def ctx k -> resolveKindConsistency def ctx k

runCheck :: Eff (Check ': '[]) a -> Either KindCheckFailure a
runCheck = run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

kindCheckType = undefined
runKindCheckEff = undefined

-- Resolvers

resolveKindConsistency tydef ctx k = do
  let
  undefined

resolveCreateContext :: forall effs. P.CompilerInput -> Eff effs Context
resolveCreateContext ci = mconcat <$> traverse module2Context (ci ^. #modules)

module2Context :: forall effs. P.Module -> Eff effs Context
module2Context m = mconcat <$> traverse (tyDef2Context (flattenModuleName (m ^. #moduleName))) (P.typeDefs m)
  where
    flattenModuleName :: P.ModuleName -> Text
    flattenModuleName mName = intercalate "." $ (\p -> p ^. #name) <$> mName ^. #parts

type ModuleName = Text

tyDef2Context :: forall effs. ModuleName -> P.TyDef -> Eff effs Context
tyDef2Context curModName tyDef = do
  let name = show $ curModName <> "." <> (tyDef ^. #tyName . #name) -- name is qualified
  let ty = tyAbsLHS2Type (tyDef ^. #tyAbs)
  pure $ mempty & context .~ M.singleton name ty

tyAbsLHS2Type :: P.TyAbs -> Kind
tyAbsLHS2Type tyAbs = foldWithArrow $ pKind2Type . (\x -> x ^. #argKind) <$> (tyAbs ^. #tyArgs)

foldWithArrow :: [Kind] -> Kind
foldWithArrow = \case [] -> Type; (x : xs) -> x :->: foldWithArrow xs

-- To Kind Conversion functions

pKind2Type :: P.Kind -> Kind
pKind2Type k =
  case k ^. #kind of
    P.KindRef P.KType -> Type
    P.KindArrow l r -> pKind2Type l :->: pKind2Type r
    _ -> error "Fixme undefined type" -- FIXME what is an undefined type meant to mean?

-- To Canonical type conversion functions.

-- | TyDef to Kind Canonical representation.
tyDef2Type :: forall eff. Member Err eff => P.TyDef -> Eff eff Type
tyDef2Type tyde = tyAbsRHS2Type (tyde ^. #tyAbs)

tyAbsRHS2Type :: forall eff. Member Err eff => P.TyAbs -> Eff eff Type
tyAbsRHS2Type tyab = tyBody2Type (tyab ^. #tyBody)

tyBody2Type :: forall eff. Member Err eff => P.TyBody -> Eff eff Type
tyBody2Type = \case
  P.OpaqueI _ -> pure $ Var "Opaque"
  P.SumI s -> sum2Type s

sum2Type :: forall eff. Member Err eff => P.Sum -> Eff eff Type
sum2Type su = foldWithSum <$> traverse constructor2Type (su ^. #constructors)

constructor2Type :: forall eff. Member Err eff => P.Constructor -> Eff eff Type
constructor2Type co = product2Type (co ^. #product)

product2Type :: forall eff. Member Err eff => P.Product -> Eff eff Type
product2Type = \case
  P.RecordI r -> record2Type r
  P.TupleI t -> tuple2Type t

record2Type :: forall eff. Member Err eff => P.Record -> Eff eff Type
record2Type r = foldWithProduct <$> traverse field2Type (r ^. #fields)

tuple2Type :: forall eff. Member Err eff => P.Tuple -> Eff eff Type
tuple2Type tu = do
  tup <- traverse ty2Type $ tu ^. #fields
  case tup of
    [] -> pure $ Var "𝟙"
    x : xs -> pure . foldWithProduct $ x :| xs

field2Type :: forall eff. Member Err eff => P.Field -> Eff eff Type
field2Type f = ty2Type (f ^. #fieldTy)

ty2Type :: forall eff. Member Err eff => P.Ty -> Eff eff Type
ty2Type = \case
  P.TyVarI tytv -> tyVar2Type tytv
  P.TyAppI tyap -> tyApp2Type tyap
  P.TyRefI tyre -> tyRef2Type tyre

tyVar2Type :: forall eff. Member Err eff => P.TyVar -> Eff eff Type
tyVar2Type tv = pure . Var . T.unpack $ (tv ^. #varName . #name)

tyApp2Type :: forall eff. Member Err eff => P.TyApp -> Eff eff Type
tyApp2Type ta = do
  fn <- ty2Type (ta ^. #tyFunc)
  args <- traverse ty2Type (ta ^. #tyArgs)
  pure $ foldWithApp (fn <| args)

tyRef2Type :: forall eff. Member Err eff => P.TyRef -> Eff eff Type
tyRef2Type = \case
  P.LocalI lref -> localTyRef2Type lref
  P.ForeignI fref -> foreignTyRef2Type fref

-- FIXME - add local module?
localTyRef2Type :: forall eff. Member Err eff => P.LocalRef -> Eff eff Type
localTyRef2Type = undefined

foreignTyRef2Type :: forall eff. Member Err eff => P.ForeignRef -> Eff eff Type
foreignTyRef2Type = undefined

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

--------------------------------------------------------------------------------
-- API
{-
-- | Main Kind Checking function
runKindCheck :: P.CompilerInput -> Either KindCheckFailure ()
runKindCheck tDefs = void $ run $ runError $ interpretKindCheck $ kindCheckDefs tDefs
-}

-- runKindCheckEff :: Eff KindCheckEff a -> Either KindCheckFailure a
-- runKindCheckEff = run . runError . interpretKindCheck

-- kindCheckType = undefined

--------------------------------------------------------------------------------
-- Strategy
{-
-- | Strategy for kind checking.
kindCheckDefs :: [PTyDef] -> Eff KindCheckEff ()
kindCheckDefs tyDefs = validateInput tyDefs >>= void . kindCheckType

kindCheckType :: [TypeDefinition] -> Eff KindCheckEff [Kind]
kindCheckType validTDef = do
  ctx <- createContext validTDef
  traverse (kindCheck ctx) validTDef

interpretKindCheck :: Eff KindCheckEff a -> Eff '[Error KindCheckFailure] a
interpretKindCheck = interpret $
  \case
    ValidateInput tDs -> validateTyDef `traverse` tDs
    CreateContext tDs -> mconcat <$> makeContext `traverse` tDs
    KindCheck ctx tD -> either (convertError tD) pure $ infer ctx (tD ^. td'sop)
  where
    convertError :: forall a. TypeDefinition -> InferErr -> Eff KindCheckFailEff a
    convertError td = throwError . InferenceFailed td

--------------------------------------------------------------------------------
-- Implementation

validateTyDef :: TyDef -> Eff KindCheckFailEff TypeDefinition
validateTyDef tD = do
  let vars = tD ^.. tyAbs . tyVars . folded . varName . name . to unpack
  sop <- go (tD ^. tyAbs . tyBody . maybe'tyBody)
  pure $
    TypeDefinition
      { _td'name = tD ^. tyName . name . to unpack
      , _td'variables = vars
      , _td'sop = foldVars vars sop
      }
  where
    go = \case
      Just body -> tyBodyToType body
      Nothing -> throwError $ InvalidProto "Type Definition must have a body"

    foldVars vs ts = case vs of
      [] -> ts
      x : xs -> Abs x (foldVars xs ts)

tyBodyToType :: TyBody'TyBody -> Eff KindCheckFailEff Type
tyBodyToType = \case
  TyBody'Opaque _ -> pure $ Var "Opaque"
  TyBody'Sum sumB -> sumToType sumB

sumToType :: Sum -> Eff KindCheckFailEff Type
sumToType sumT = do
  let _constrNames :: [String] = sumT ^.. constructors . folded . constrName . name . to unpack
      products :: [Maybe Product'Product] = sumT ^.. constructors . folded . PF.product . maybe'product
  sumTRes <-
    for
      products
      $ \case
        Just (Product'Ntuple nt) -> nTupleToType nt
        Just (Product'Record' re) -> recordToType re
        Nothing -> throwError $ InvalidProto "Every constructor should have a product defining it"
  foldWithEither sumTRes

nTupleToType :: Product'NTuple -> Eff KindCheckFailEff Type
nTupleToType nt = do
  let fs :: [Ty] = nt ^. fields
  prodT <- tyToType `traverse` fs
  foldWithTuple prodT

recordToType :: Product'Record -> Eff KindCheckFailEff Type
recordToType rcrd = do
  let x :: [Ty] = rcrd ^.. fields . folded . fieldTy
  tC <- tyToType `traverse` x
  foldWithTuple tC

-- old version: foldr ($) (Var "()") . fmap (App . App (Var "(,)"))
foldWithTuple :: [Type] -> Eff KindCheckFailEff Type
foldWithTuple = foldWithBinaryOp $ App . App (Var "(,)")

-- old version foldr ($) (Var "Void") . fmap (App . App (Var "Either"))
foldWithEither :: [Type] -> Eff KindCheckFailEff Type
foldWithEither = foldWithBinaryOp $ App . App (Var "Either")

-- | Generic way of folding.
foldWithBinaryOp :: (Type -> Type -> Type) -> [Type] -> Eff KindCheckFailEff Type
foldWithBinaryOp op = \case
  [] -> throwError InvalidType
  [x] -> pure x
  x : xs -> op x <$> foldWithTuple xs

tyToType :: Ty -> Eff KindCheckFailEff Type
tyToType ty = do
  case ty ^. maybe'ty of
    Just (Ty'TyVar v) -> pure $ Var (v ^. varName . name . to unpack)
    Just (Ty'TyApp app) -> tyAppToType app
    Just (Ty'TyRef ref) -> tyRefToType ref
    Nothing -> throwError $ InvalidProto "Type is not defined"

tyAppToType :: TyApp -> Eff KindCheckFailEff Type
tyAppToType tApp = do
  fC <- tyToType (tApp ^. tyFunc)
  fArgsC <- tyToType `traverse` (tApp ^. tyArgs)
  case fArgsC of
    [] -> throwError AppNoArgs
    _ -> pure $ appF fC fArgsC
  where
    appF t [] = t
    appF t (x : xs) = appF (App t x) xs

makeContext :: TypeDefinition -> Eff KindCheckFailEff Context
makeContext td =
  pure $ mempty & context .~ [(td ^. td'name, g (td ^. td'variables))]
  where
    g :: [a] -> Kind
    g = \case
      [] -> Type
      (_ : xs) -> Type :->: g xs

tyRefToType :: TyRef -> Eff KindCheckFailEff Type
tyRefToType tR = do
  case tR ^. maybe'tyRef of
    Just (TyRef'LocalTyRef t) -> pure $ Var $ t ^. tyName . name . to unpack
    Just (TyRef'ForeignTyRef t) -> pure $ Var $ (t ^. moduleName . parts . to (\ps -> unpack $ intercalate "." [p ^. name | p <- ps])) <> "." <> (t ^. tyName . name . to unpack)
    Nothing -> throwError $ InvalidProto "TyRef Cannot be empty"
-}
