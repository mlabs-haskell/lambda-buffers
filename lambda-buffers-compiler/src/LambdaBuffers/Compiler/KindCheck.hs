{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LambdaBuffers.Compiler.KindCheck (runKindCheck) where

import Control.Exception (Exception)
import Control.Lens (Getter, Identity (runIdentity), folded, folding, makeLenses, mapped, to, view, (&), (.~), (^.), (^..), _Just)
import Control.Monad.Freer (Eff, Members, interpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.State (get, modify)
import Control.Monad.Freer.TH (makeEffect)
import Data.String ()
import Data.Text (Text, unpack)
import LambdaBuffers.Compiler.KindCheck.Inference (
  Atom,
  Context (context),
  DError,
  DeriveEff,
  DeriveM,
  Kind (Type, (:->:)),
  Type (..),
  infer,
 )

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Proto.Compiler (
  ConstrName,
  Product,
  Product'NTuple,
  Product'Product (Product'Empty', Product'Ntuple, Product'Record'),
  Product'Record,
  Sum,
  Sum'Constructor,
  Ty,
  Ty'Ty (Ty'TyApp, Ty'TyRef, Ty'TyVar),
  TyApp,
  TyBody'TyBody (TyBody'Opaque, TyBody'Sum),
  TyDef,
  TyRef,
 )
import Proto.Compiler_Fields as PF (
  constrName,
  constructors,
  fields,
  maybe'empty,
  maybe'product,
  maybe'ty,
  maybe'tyBody,
  name,
  product,
  tyAbs,
  tyBody,
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
  deriving stock (Show, Eq)

instance Exception KindCheckFailure

-- | Validated Type Definition.
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
runKindCheck tDefs = void $ run $ runError $ runKindCheckEff $ kindCheckDefs tDefs
  where
    -- Strategy for kind checking.
    kindCheckDefs :: [TyDef] -> Eff KindCheckEff ()
    kindCheckDefs tyDefs = do
      validTDef <- validateInput tyDefs
      ctx <- createContext validTDef
      traverse_ (kindCheck ctx) validTDef

    -- Interpreting the effect.
    runKindCheckEff :: Eff KindCheckEff a -> Eff '[Error KindCheckFailure] a
    runKindCheckEff = interpret $
      \case
        ValidateInput tDs -> validateTyDef `traverse` tDs
        CreateContext tDs -> mconcat <$> makeContext `traverse` tDs
        KindCheck ctx tD -> either convertError pure $ infer ctx (toTerms tD)

    -- Error converter. :fixme:
    convertError :: forall a. DError -> Eff KindCheckFailEff a
    convertError e = throwError $ CheckFailure $ show e

--------------------------------------------------------------------------------
-- Implementations

validateTyDef :: TyDef -> Eff KindCheckFailEff TypeDefinition
validateTyDef tD = do
  sop <- go (tD ^. tyAbs . tyBody . maybe'tyBody)
  pure $
    TypeDefinition
      { _td'name = tD ^. tyName . name . to unpack
      , _td'variables = view (varName . name . to unpack) <$> (tD ^. tyAbs . tyVars)
      , _td'sop = sop
      }
  where
    go = \case
      Just (TyBody'Opaque _) -> pure $ Var "Opaque"
      Just (TyBody'Sum sumB) -> sumToType sumB
      Nothing -> throwError $ InvalidProto "Type Definition must have a body"

sumToType :: Sum -> Eff KindCheckFailEff Type
sumToType sumT = do
  let _constrNames :: [String] = sumT ^.. constructors . folded . constrName . name . to unpack
      products :: [Maybe Product'Product] = sumT ^.. constructors . folded . PF.product . maybe'product
  sumTRes <-
    for
      products
      $ \case
        Just (Product'Empty' _) -> pure $ Var "()"
        Just (Product'Ntuple nt) -> nTupleToType nt
        Just (Product'Record' re) -> recordToType re
        Nothing -> throwError $ InvalidProto "Every constructor should have a product defining it"

  -- :fixme: needs to be tested
  pure $ foldr ($) (Var "Void") $ fmap (App . App (Var "Either")) sumTRes

nTupleToType :: Product'NTuple -> Eff KindCheckFailEff Type
nTupleToType nt = do
  let fs :: [Ty] = nt ^. fields
  prodT <- tyToType `traverse` fs
  -- :fixme: needs to be tested
  pure $ foldr ($) (Var "()") $ fmap (App . App (Var "(,)")) prodT

tyToType :: Ty -> Eff KindCheckFailEff Type
tyToType ty = do
  case ty ^. maybe'ty of
    Just (Ty'TyVar v) -> pure $ Var (v ^. varName . name . to unpack)
    Just (Ty'TyApp app) -> tyAppToType app
    Just (Ty'TyRef _) -> undefined
    Nothing -> throwError $ InvalidProto "Type is not defined"

tyAppToType :: TyApp -> Eff KindCheckFailEff Type
tyAppToType = undefined

recordToType :: Product'Record -> Eff KindCheckFailEff Type
recordToType = undefined

makeContext :: TypeDefinition -> Eff KindCheckFailEff Context
makeContext = undefined

toTerms :: TypeDefinition -> Type
toTerms = undefined

tyRefToType :: TyRef -> Eff KindCheckFailEff Type
tyRefToType = undefined

{-
-- Getting to localise potential API changes in the protos file.
defToType :: Getter TyDef TypeDefinition
defToType = to getter
  where
    getter :: TyDef -> TypeDefinition
    getter td =
      TypeDefinition
        { _td'name = td ^. tyName . name . to unpack
        , _td'variables = view (varName . name . to unpack) <$> (td ^. tyAbs . tyVars)
        , _td'constructors = go (td ^. tyAbs . tyBody . maybe'tyBody)
        }

    go = \case
      Just x -> case x of
        TyBody'Opaque _ -> [] -- we do not need to TC Opaques?
        TyBody'Sum s ->
          let constrs = s ^. constructors
          in fmap (\e -> (,)
                    (e ^. constrName . name . to unpack)
                    (productToType (e ^. PF.product . maybe'product))
                  ) constrs
      Nothing -> error ":FIXME: handle errors"
      where
        productToType :: Maybe Product'Product -> Type
        productToType = \case
          Just x -> case x of
            Product'Empty' _ -> Type

getLHS :: Getter TypeDefinition (Atom, Kind)
getLHS = to getter
  where
    getter td = (td ^. td'name, foldr (:->:) Type tV)
      where
        tV = Type <$ (td ^. td'variables)

getRHS :: Getter TypeDefinition [(Atom, Type)]
getRHS = to getter
  where
    getter td = undefined
      where
        tv :: Type -> Type
        tv = foldr1 (.) $ Abs <$> (td ^. td'variables)
-}
