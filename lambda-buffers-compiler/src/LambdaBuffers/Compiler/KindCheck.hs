module LambdaBuffers.Compiler.KindCheck (runKindCheck) where

import Control.Exception (Exception)
import Control.Lens (Identity (runIdentity), (&), (.~), (^.))
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (Except, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (local)
import Data.Foldable (Foldable (foldl'), for_)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Base qualified as GHC
import Proto.Compiler (Opaque, Product, Product'Product (Product'Empty', Product'Ntuple, Product'Record'), Sum, Ty, Ty'Ty (Ty'TyApp, Ty'TyRef, Ty'TyVar), TyAbs, TyApp, TyBody, TyBody'TyBody (TyBody'Opaque, TyBody'Sum), TyDef, TyRef, TyVar)
import Proto.Compiler_Fields as ProtoFields (
  constructors,
  fieldTy,
  fields,
  foreignTyRef,
  maybe'product,
  maybe'ty,
  maybe'tyBody,
  moduleName,
  name,
  product,
  tyAbs,
  tyArgs,
  tyBody,
  tyFunc,
  tyName,
  tyVars,
  varName,
 )

-- A LB Kind is either a -> or *, but where KindRef "Type" == *
-- TODO: Add Kind to compiler.proto
type Kind :: Type
data Kind = KindRef String | KindArrow Kind Kind deriving stock (Eq)

typeK :: Kind
typeK = KindRef "Type"

arrowK :: Kind -> Kind -> Kind
arrowK = KindArrow

instance Show Kind where
  show (KindArrow l r) = "(" <> show l <> " -> " <> show r <> ")"
  show (KindRef "Type") = "*"
  show (KindRef refName) = refName

-- TODO: Add KindCheckFailure to compiler.proto
type KindCheckFailure :: Type
data KindCheckFailure
  = CheckFailure String
  | LookupVarFailure Text
  | LookupRefFailure TyRef
  | AppWrongArgKind Kind Kind -- Expected Kind got Kind
  | AppToManyArgs Int
  | InvalidProto Text
  deriving stock (Show, Eq)

instance Exception KindCheckFailure

-- TODO: Extend with SourceInfo and Sum'Constructor information so we can emit errors with the entire env
-- TODO: KindCheckEnvironment should also go to compiler.proto and the Frontend can use that for error reporting
type KindCheckEnv :: Type
data KindCheckEnv = KCEnv
  { variables :: Map Text Kind
  , refs :: Map TyRef Kind
  }
  deriving stock (Show, Eq)

type KindCheckMonad :: Type -> Type
type KindCheckMonad = ReaderT KindCheckEnv (Except KindCheckFailure)

runKindCheck :: KindCheckMonad Kind -> Either KindCheckFailure Kind
runKindCheck p = runIdentity . runExceptT $ runReaderT p defKCEnv

applyK :: Kind -> [Kind] -> KindCheckMonad Kind
applyK (KindRef _) args@(_ : _) = lift . throwE $ AppToManyArgs (length args)
applyK k [] = return k
applyK (KindArrow l r) (arg : args) = if l == arg then applyK r args else lift . throwE $ AppWrongArgKind l arg

type TyKind :: Type -> GHC.Constraint
class TyKind ty where
  tyKind :: ty -> KindCheckMonad Kind

instance TyKind TyDef where
  tyKind tyD = tyKind $ tyD ^. tyAbs

instance TyKind TyAbs where
  tyKind tyAbs' = do
    kBody <- extendVarEnv (tyAbs' ^. tyVars) (tyKind $ tyAbs' ^. tyBody)
    let k = foldl' (\kf _ -> arrowK typeK . kf) id (tyAbs' ^. tyVars)
    return $ k kBody

instance TyKind TyApp where
  tyKind tyA = do
    ktFun <- tyKind $ tyA ^. tyFunc
    ktArgs <- for (tyA ^. tyArgs) tyKind
    applyK ktFun ktArgs

instance TyKind TyVar where
  tyKind tyV = lookupVar $ tyV ^. varName . name

instance TyKind TyRef where
  tyKind = lookupRef

instance TyKind TyBody where
  tyKind tyB = case tyB ^. maybe'tyBody of
    Nothing -> lift . throwE $ InvalidProto "Must have TyBody"
    Just tyB' -> case tyB' of
      TyBody'Opaque o -> tyKind o
      TyBody'Sum s -> tyKind s

instance TyKind Opaque where
  tyKind _ = return typeK -- NOTE: This could be configurable and thus could enable arbitrary kinded Opaques

instance TyKind Sum where
  tyKind s = do
    for_ (s ^. constructors) (\c -> tyKind $ c ^. ProtoFields.product)
    return typeK -- NOTE: Sum bodies are * kinded

instance TyKind Product where
  tyKind p = case p ^. maybe'product of
    Nothing -> lift . throwE $ InvalidProto "Must have TyProduct"
    Just p' -> case p' of
      Product'Empty' _ -> return typeK
      Product'Record' r -> do
        for_
          (r ^. fields)
          ( \f -> do
              k <- tyKind $ f ^. fieldTy
              if k == typeK then return () else lift . throwE $ CheckFailure "All fields in a record must have kind *" -- TODO: Add separate error
          )
        return typeK
      Product'Ntuple n -> do
        for_
          (n ^. fields)
          ( \ty -> do
              k <- tyKind ty
              if k == typeK then return () else lift . throwE $ CheckFailure "All fields in a tuple must have kind *" -- TODO: Add separate error
          )
        return typeK -- -- NOTE: Product bodies are * kinded

instance TyKind Ty where
  tyKind ty = case ty ^. maybe'ty of
    Nothing -> lift . throwE $ InvalidProto "Must have either TyApp, TyVar or TyRef"
    Just ty' -> case ty' of
      Ty'TyVar tyV -> tyKind tyV
      Ty'TyApp tyA -> tyKind tyA
      Ty'TyRef tyR -> tyKind tyR

lookupVar :: Text -> KindCheckMonad Kind
lookupVar v = do
  mb <- asks (Map.lookup v . variables)
  case mb of
    Just t -> return t
    Nothing -> lift . throwE $ LookupVarFailure v

extendVarEnv :: [TyVar] -> KindCheckMonad a -> KindCheckMonad a
extendVarEnv vars = local (\(KCEnv vs refs) -> KCEnv (insertVars vs) refs)
  where
    insertVars vs = foldr (\tyV envVars -> Map.insert (tyV ^. varName . name) typeK envVars) vs vars

lookupRef :: TyRef -> KindCheckMonad Kind
lookupRef r = do
  mb <- asks (Map.lookup r . refs)
  case mb of
    Just t -> return t
    Nothing -> lift . throwE $ LookupRefFailure r

defKCEnv :: KindCheckEnv
defKCEnv =
  KCEnv Map.empty $
    Map.fromList
      [ (lbPreludeRef "Either", typeK `arrowK` (typeK `arrowK` typeK))
      , (lbPreludeRef "Maybe", typeK `arrowK` typeK)
      , (lbPreludeRef "Tuple", typeK `arrowK` (typeK `arrowK` typeK))
      , (lbPreludeRef "Int", typeK)
      , (lbPreludeRef "Bool", typeK)
      , (lbPreludeRef "Map", typeK `arrowK` (typeK `arrowK` typeK))
      , (lbPreludeRef "List", typeK `arrowK` typeK)
      ]
  where
    lbPreludeRef :: Text -> TyRef
    lbPreludeRef tyN =
      defMessage
        & foreignTyRef . moduleName . name .~ "Prelude"
        & foreignTyRef . tyName . name .~ tyN

-- main :: IO ()
-- main =
--   void $
--     runTestTT $
--       TestList
--         [ TestLabel "should succeed" $
--             TestList
--               [ ae "\\s -> Maybe s :: * -> *" (TyAbs "s" (TyApp (TyRef "Maybe") (TyVar "s"))) "(* -> *)"
--               , ae "\\s -> Maybe :: * -> (* -> *)" (TyAbs "s" (TyRef "Maybe")) "(* -> (* -> *))"
--               , ae "Maybe Int :: *" (TyApp (TyRef "Maybe") (TyRef "Int")) "*"
--               , ae "\\s -> s :: * -> *" (TyAbs "s" (TyVar "s")) "(* -> *)"
--               , ae
--                   "\\a b -> Either a b :: * -> *"
--                   ( TyAbs
--                       "a"
--                       ( TyAbs
--                           "b"
--                           ( TyApp
--                               (TyApp (TyRef "Either") (TyVar "a"))
--                               (TyVar "b")
--                           )
--                       )
--                   )
--                   "(* -> (* -> *))"
--               , ae
--                   "\\a b -> Either :: * -> * -> (* -> * -> *)"
--                   ( TyAbs
--                       "a"
--                       ( TyAbs
--                           "b"
--                           (TyRef "Either")
--                       )
--                   )
--                   "(* -> (* -> (* -> (* -> *))))"
--               , ae
--                   "\\a b -> Either (Maybe a) (Maybe b):: * -> * -> *"
--                   ( TyAbs
--                       "a"
--                       ( TyAbs
--                           "b"
--                           ( TyApp
--                               ( TyApp
--                                   (TyRef "Either")
--                                   (TyApp (TyRef "Maybe") (TyVar "a"))
--                               )
--                               (TyApp (TyRef "Maybe") (TyVar "b"))
--                           )
--                       )
--                   )
--                   "(* -> (* -> *))"
--               ]
--         , TestLabel "should fail" $
--             TestList
--               [ af "\\s -> Maybe a" (TyAbs "s" (TyApp (TyRef "Maybe") (TyVar "a"))) (LookupVarFailure "a")
--               , af "\\s -> Naybe s" (TyAbs "s" (TyApp (TyRef "Naybe") (TyVar "s"))) (LookupRefFailure "Naybe")
--               , af "\\s -> s s" (TyAbs "s" (TyApp (TyVar "s") (TyVar "s"))) (LookupRefFailure "Naybe") -- TODO
--               ]
--         ]
--   where
--     ae s ty k = TestCase $ assertEqual s (go ty) (Right k)

--     af s ty err = TestCase $ assertEqual s (go ty) (Left err)

--     go :: Ty -> Either KindCheckFailure String
--     go ty = case runKindCheck $ checkKind ty >>= (lift . applyBindings) of
--       Nothing -> Left $ CheckFailure ""
--       Just (e, _) -> case e of
--         Left kcf -> Left kcf
--         Right ut -> Right $ show ut
