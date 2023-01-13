{-# LANGUAGE TemplateHaskell #-}

{- | Note: At the moment the Kind Checker disregards multiple Modules for
simplicity of testing and developing. This will be changed ASAP. :fixme:
-}
module LambdaBuffers.Compiler.KindCheck (
  KindCheckFailure (..),
  runKindCheck,
  TypeDefinition (..),

  -- * Testing Utils
  kindCheckType,
  runKindCheckEff,
) where

import Control.Exception (Exception)
import Control.Lens (folded, makeLenses, to, (&), (.~), (^.), (^..))
import Control.Monad.Freer (Eff, interpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.TH (makeEffect)
import Data.Text (Text, intercalate, unpack)
import LambdaBuffers.Compiler.KindCheck.Inference (
  Context,
  InferErr,
  Kind (Type, (:->:)),
  Type (Abs, App, Var),
  context,
  infer,
 )

import Control.Monad (void)
import Data.Traversable (for)
import Proto.Compiler (
  Product'NTuple,
  Product'Product (Product'Ntuple, Product'Record'),
  Product'Record,
  Sum,
  Ty,
  Ty'Ty (Ty'TyApp, Ty'TyRef, Ty'TyVar),
  TyApp,
  TyBody'TyBody (TyBody'Opaque, TyBody'Sum),
  TyDef,
  TyRef,
  TyRef'TyRef (TyRef'ForeignTyRef, TyRef'LocalTyRef),
 )
import Proto.Compiler_Fields as PF (
  constrName,
  constructors,
  fieldTy,
  fields,
  maybe'product,
  maybe'ty,
  maybe'tyBody,
  maybe'tyRef,
  moduleName,
  name,
  parts,
  product,
  tyAbs,
  tyArgs,
  tyBody,
  tyFunc,
  tyName,
  tyVars,
  varName,
 )

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
data KindCheckFailure
  = CheckFailure String
  | LookupVarFailure Text
  | LookupRefFailure TyRef
  | AppWrongArgKind Kind Kind -- Expected Kind got Kind
  | AppToManyArgs Int
  | InvalidProto Text
  | AppNoArgs -- No args
  | InvalidType
  | InferenceFailed TypeDefinition InferErr
  deriving stock (Show, Eq)

instance Exception KindCheckFailure

{- | Validated Type Definition.
 :fixme: Add to compiler.proto
-}
data TypeDefinition = TypeDefinition
  { _td'name :: String
  , _td'variables :: [String]
  , _td'sop :: Type
  }
  deriving stock (Show, Eq)

makeLenses 'TypeDefinition

data KindCheck a where
  ValidateInput :: [TyDef] -> KindCheck [TypeDefinition]
  CreateContext :: [TypeDefinition] -> KindCheck Context
  KindCheck :: Context -> TypeDefinition -> KindCheck Kind

makeEffect ''KindCheck

type KindCheckFailEff = '[Error KindCheckFailure]
type KindCheckEff = KindCheck ': KindCheckFailEff

--------------------------------------------------------------------------------
-- API

-- | Main Kind Checking function
runKindCheck :: [TyDef] -> Either KindCheckFailure ()
runKindCheck tDefs = void $ run $ runError $ interpretKindCheck $ kindCheckDefs tDefs

runKindCheckEff :: Eff KindCheckEff a -> Either KindCheckFailure a
runKindCheckEff = run . runError . interpretKindCheck

--------------------------------------------------------------------------------
-- Strategy

-- | Strategy for kind checking.
kindCheckDefs :: [TyDef] -> Eff KindCheckEff ()
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
