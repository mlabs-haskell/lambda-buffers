{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module LambdaBuffers.Compiler.KindCheck (runKindCheck, tyDefKind) where

import Control.Exception (Exception)
import Control.Lens (Getter, Identity (runIdentity), makeLenses, to, view, (^.))
import Control.Monad.Freer (Eff, interpret)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.State (get, modify)
import Control.Monad.Freer.TH (makeEffect)
import Data.String ()
import Data.Text (Text, unpack)
import LambdaBuffers.Compiler.KindCheck.Inference (
  Atom,
  Context (context),
  DeriveEff,
  DeriveM,
  Kind (Type, (:->:)),
  Type (Abs),
  infer,
 )

import Proto.Compiler (TyBody'TyBody (TyBody'Opaque, TyBody'Sum), TyDef, TyRef)
import Proto.Compiler_Fields as ProtoFields (
  maybe'tyBody,
  name,
  tyAbs,
  tyBody,
  tyName,
  tyVars,
  varName,
 )

data KindCheckFailure
  = CheckFailure String
  | LookupVarFailure Text
  | LookupRefFailure TyRef
  | AppWrongArgKind Kind Kind -- Expected Kind got Kind
  | AppToManyArgs Int
  | InvalidProto Text
  deriving stock (Show, Eq)

instance Exception KindCheckFailure

type KindCheckMonad a = Identity a

runKindCheck :: KindCheckMonad Kind -> Either KindCheckFailure Kind
runKindCheck p = undefined -- runIdentity . runExceptT $ runReaderT p defKCEnv

data TypeDefinition = TypeDefinition
  { _td'name :: String
  , _td'variables :: [String]
  , _td'constructors :: [(Atom, Type)]
  }
  deriving stock (Show, Eq)

makeLenses 'TypeDefinition

data KindCheck a where
  Check :: Type -> KindCheck Kind
  AddToContext :: (Atom, Kind) -> KindCheck ()
  CheckError :: KindCheckFailure -> KindCheck ()

makeEffect ''KindCheck

type KindChecker a = DeriveM a

runKindCheck' :: Eff (KindCheck ': Error KindCheckFailure ': DeriveEff) a -> Eff (Error KindCheckFailure ': DeriveEff) a
runKindCheck' = interpret f
  where
    f :: KindCheck a -> Eff (Error KindCheckFailure ': DeriveEff) a
    f = \case
      Check t -> do
        ctx <- get @Context
        case infer ctx t of
          Right k -> pure k
          Left e -> throwError $ CheckFailure $ show e
      AddToContext def ->
        modify @Context (\ctx -> ctx {context = def : context ctx})
      CheckError e -> throwError e

-- | Kind Check one definition - against the already type checked definitions.
kindCheckDef :: TyDef -> Eff '[KindCheck] [Kind]
kindCheckDef tDef = do
  let lhs = tDef ^. defToType . getLHS
  let rhs = tDef ^. defToType . getRHS
  addToContext lhs
  traverse (check . snd) rhs

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
        TyBody'Opaque o -> []
        TyBody'Sum s -> []
      Nothing -> error ":FIXME:"

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

tyDefKind :: TyDef -> TypeDefinition
tyDefKind = view defToType

{-
tyAbsKind :: TyAbs -> KindCheckMonad Kind
tyAbsKind tyAbs' = do
  kBody <- extendVarEnv (tyAbs' ^. tyVars) (tyBodyKind $ tyAbs' ^. tyBody)
  let k = foldl' (\kf _ -> arrowK typeK . kf) id (tyAbs' ^. tyVars)
  return $ k kBody

tyAppKind :: TyApp -> KindCheckMonad Kind
tyAppKind tyA = do
  ktFun <- tyKind $ tyA ^. tyFunc
  ktArgs <- for (tyA ^. tyArgs) tyKind
  applyK ktFun ktArgs

tyVarKind :: TyVar -> KindCheckMonad Kind
tyVarKind tyV = lookupVar $ tyV ^. varName . name

tyRefKind :: TyRef -> KindCheckMonad Kind
tyRefKind = lookupRef

tyBodyKind :: TyBody -> KindCheckMonad Kind
tyBodyKind tyB = case tyB ^. maybe'tyBody of
  Nothing -> lift . throwE $ InvalidProto "Must have TyBody"
  Just tyB' -> case tyB' of
    TyBody'Opaque o -> tyBodyOpaqueKind o
    TyBody'Sum s -> tyBodySumKind s

tyBodyOpaqueKind :: Opaque -> KindCheckMonad Kind
tyBodyOpaqueKind _ = return typeK -- NOTE: This could be configurable and thus could enable arbitrary kinded Opaques

tyBodySumKind :: Sum -> KindCheckMonad Kind
tyBodySumKind s = do
  for_ (s ^. constructors) (\c -> tyProductKind $ c ^. ProtoFields.product)
  return typeK -- NOTE: Sum bodies are * kinded

tyProductKind :: Product -> KindCheckMonad Kind
tyProductKind p = case p ^. maybe'product of
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

tyKind :: Ty -> KindCheckMonad Kind
tyKind ty = case ty ^. maybe'ty of
  Nothing -> lift . throwE $ InvalidProto "Must have either TyApp, TyVar or TyRef"
  Just ty' -> case ty' of
    Ty'TyVar tyV -> tyVarKind tyV
    Ty'TyApp tyA -> tyAppKind tyA
    Ty'TyRef tyR -> tyRefKind tyR

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
-}
