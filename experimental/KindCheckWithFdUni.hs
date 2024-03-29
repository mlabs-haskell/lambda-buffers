{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KindCheckWithFdUni where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception, throw)
import Control.Monad (void)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logic (Logic, runLogic)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT, local)
import Control.Unification (BindingMonad (freeVar, newVar), Fallible (mismatchFailure, occursFailure), UTerm (UTerm, UVar), Unifiable (zipMatch), applyBindings, applyBindingsAll, unify, (=:=))
import Control.Unification.IntVar (IntBindingState, IntBindingT, IntVar, runIntBindingT)
import Control.Unification.Types (UFailure, UTerm)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)

-- A toy Type expression
data Ty = TyVar String | TyAbs String Ty | TyApp Ty Ty | TyRef String deriving (Eq, Show)

-- A LB Kind is either a -> or *, but where KindRef "Type" == *
data Kind a = KindRef String | KindArrow a a deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (Kind a) where
  show (KindArrow l r) = "(" <> show l <> " -> " <> show r <> ")"
  show (KindRef "Type") = "*"
  show (KindRef refName) = refName

-- fd-unification typeclass to define how 'one level' of a Kind is unified
instance Unifiable Kind where
  zipMatch :: Kind a -> Kind a -> Maybe (Kind (Either a (a, a)))
  zipMatch (KindRef r :: Kind a) (KindRef r') = if r == r' then Just $ KindRef r else Nothing
  zipMatch (KindArrow a b) (KindArrow a' b') = Just $ KindArrow (Right (a, a')) (Right (b, b'))
  zipMatch _ _ = Nothing

-- fd-unification wiring
-- KindTerm that is being checked via unification
type KindTerm = UTerm Kind IntVar

instance Eq KindTerm where
  (==) :: KindTerm -> KindTerm -> Bool
  UTerm x == UTerm y = x == y
  UVar x == UVar y = x == y
  _ == _ = False

data KindCheckFailure
  = OccursFailure IntVar KindTerm
  | MismatchFailure (Kind KindTerm) (Kind KindTerm)
  | CheckFailure String
  | LookupVarFailure String
  | LookupRefFailure String
  deriving (Show, Eq)

instance Fallible Kind IntVar KindCheckFailure where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

instance Exception KindCheckFailure

type KindCheckBindingState = IntBindingState Kind

data KindCheckEnv = KCEnv
  { variables :: Map String KindTerm
  , refs :: Map String KindTerm
  }
  deriving (Show, Eq)

type KindCheckMonad = ReaderT KindCheckEnv (ExceptT KindCheckFailure (IntBindingT Kind Logic))

runKindCheck ::
  KindCheckMonad a ->
  Maybe (Either KindCheckFailure a, KindCheckBindingState)
runKindCheck p = observeMaybe . runIntBindingT . runExceptT $ runReaderT p defKCEnv

observeMaybe :: Logic a -> Maybe a
observeMaybe mx = runLogic mx (\a _ -> Just a) Nothing

-- Kind *
typeKind :: UTerm Kind v
typeKind = UTerm (KindRef "Type")

-- Kind ->
kindArrow :: UTerm Kind v -> UTerm Kind v -> UTerm Kind v
kindArrow l r = UTerm (KindArrow l r)

unify' :: KindTerm -> KindTerm -> KindCheckMonad KindTerm
unify' l r = lift $ unify l r

freeVar' :: KindCheckMonad KindTerm
freeVar' = lift . lift $ UVar <$> freeVar

checkKind :: Ty -> KindCheckMonad KindTerm
checkKind (TyVar s) = lookupVar s
checkKind (TyApp tyFun tyArg) = do
  -- ty_kind(Ctx, ty_app(TyFun, TyArg), KtApp) :-
  --     ty_kind(Ctx, TyFun, KtFun),
  --     ty_kind(Ctx, TyArg, KtArg),
  --     KtFun = arr(KtArg, KtApp).
  ktFun <- checkKind tyFun
  ktArg <- checkKind tyArg
  ktApp <- freeVar'
  ktFun `unify'` kindArrow ktArg ktApp -- {KtFun, KtArg} ? KtFun = KtArg -> KtApp
  return ktApp
checkKind (TyAbs varName tyBody) = do
  -- ty_kind(ctx(Vars, Refs), ty_abs(VarName, TyBody), KtAbs) :-
  --     KtVar = "*",
  --     append([VarName-KtVar], Vars, Vars_),
  --     ty_kind(ctx(Vars_, Refs), TyBody, KtBody),
  --     KtAbs = arr(KtVar, KtBody).
  ktVar <- freeVar'
  ktVar `unify'` typeKind -- all vars are of kind *, KtVar = *
  ktBody <- extendVarEnv varName ktVar (checkKind tyBody)
  ktAbs <- freeVar'
  ktAbs `unify'` kindArrow ktVar ktBody -- {KtVar, KtBody} ? KtAbs = KtVar -> KtBody
  return ktAbs
checkKind (TyRef s) = lookupRef s

lookupVar :: String -> KindCheckMonad KindTerm
lookupVar varName = do
  mb <- asks (Map.lookup varName . variables)
  case mb of
    Just t -> return t
    Nothing -> lift . throwE $ LookupVarFailure varName

extendVarEnv :: String -> KindTerm -> KindCheckMonad a -> KindCheckMonad a
extendVarEnv varName kt = local (\(KCEnv vs refs) -> KCEnv (Map.insert varName kt vs) refs)

lookupRef :: String -> KindCheckMonad KindTerm
lookupRef refName = do
  mb <- asks (Map.lookup refName . refs)
  case mb of
    Just t -> return t
    Nothing -> lift . throwE $ LookupRefFailure refName

defKCEnv :: KindCheckEnv
defKCEnv =
  KCEnv Map.empty $
    Map.fromList
      [ ("Either", typeKind `kindArrow` (typeKind `kindArrow` typeKind))
      , ("Maybe", typeKind `kindArrow` typeKind)
      , ("Tuple", typeKind `kindArrow` (typeKind `kindArrow` typeKind))
      , ("Int", typeKind)
      , ("Bool", typeKind)
      , ("Map", typeKind `kindArrow` (typeKind `kindArrow` typeKind))
      , ("List", typeKind `kindArrow` typeKind)
      ]

main :: IO ()
main =
  void $
    runTestTT $
      TestList
        [ TestLabel "should succeed" $
            TestList
              [ ae "\\s -> Maybe s :: * -> *" (TyAbs "s" (TyApp (TyRef "Maybe") (TyVar "s"))) "(* -> *)"
              , ae "\\s -> Maybe :: * -> (* -> *)" (TyAbs "s" (TyRef "Maybe")) "(* -> (* -> *))"
              , ae "Maybe Int :: *" (TyApp (TyRef "Maybe") (TyRef "Int")) "*"
              , ae "\\s -> s :: * -> *" (TyAbs "s" (TyVar "s")) "(* -> *)"
              , ae
                  "\\a b -> Either a b :: * -> *"
                  ( TyAbs
                      "a"
                      ( TyAbs
                          "b"
                          ( TyApp
                              (TyApp (TyRef "Either") (TyVar "a"))
                              (TyVar "b")
                          )
                      )
                  )
                  "(* -> (* -> *))"
              , ae
                  "\\a b -> Either :: * -> * -> (* -> * -> *)"
                  ( TyAbs
                      "a"
                      ( TyAbs
                          "b"
                          (TyRef "Either")
                      )
                  )
                  "(* -> (* -> (* -> (* -> *))))"
              , ae
                  "\\a b -> Either (Maybe a) (Maybe b):: * -> * -> *"
                  ( TyAbs
                      "a"
                      ( TyAbs
                          "b"
                          ( TyApp
                              ( TyApp
                                  (TyRef "Either")
                                  (TyApp (TyRef "Maybe") (TyVar "a"))
                              )
                              (TyApp (TyRef "Maybe") (TyVar "b"))
                          )
                      )
                  )
                  "(* -> (* -> *))"
              ]
        , TestLabel "should fail" $
            TestList
              [ af "\\s -> Maybe a" (TyAbs "s" (TyApp (TyRef "Maybe") (TyVar "a"))) (LookupVarFailure "a")
              , af "\\s -> Naybe s" (TyAbs "s" (TyApp (TyRef "Naybe") (TyVar "s"))) (LookupRefFailure "Naybe")
              , af "\\s -> s s" (TyAbs "s" (TyApp (TyVar "s") (TyVar "s"))) (LookupRefFailure "Naybe") -- TODO
              ]
        ]
  where
    ae s ty k = TestCase $ assertEqual s (go ty) (Right k)

    af s ty err = TestCase $ assertEqual s (go ty) (Left err)

    go :: Ty -> Either KindCheckFailure String
    go ty = case runKindCheck $ checkKind ty >>= (lift . applyBindings) of
      Nothing -> Left $ CheckFailure ""
      Just (e, _) -> case e of
        Left kcf -> Left kcf
        Right ut -> Right $ show ut
