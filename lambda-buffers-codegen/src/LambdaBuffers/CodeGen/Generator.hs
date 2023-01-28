{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds, PolyKinds, GADTs, RankNTypes, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module LambdaBuffers.CodeGen.Generator where

import Data.List ( nub, foldl' )
import Control.Applicative (Alternative(..))
import Data.Kind
import Prettyprinter (Doc)

import LambdaBuffers.Common.TypeClass.Match
import LambdaBuffers.Common.TypeClass.Pat

data Lang where
  Rust :: Lang
  PureScript :: Lang
  Haskell :: Lang
  Plutarch :: Lang
  deriving stock (Show, Eq)

data GenComponent
  = TypeDecl
  | InstanceDecl
  | ImportDecl

class TargetLang (l :: Lang) where
  type DSL l :: Type -- can change to something else later

  toDoc :: DSL l -> Doc ()

instance TargetLang Rust where
  type DSL Rust = Doc ()

  toDoc = id

instance TargetLang Haskell where
  type (DSL Haskell) = Doc ()

  toDoc = id

-- adapted from https://serokell.io/blog/parser-combinators-in-haskell
data Error e
  = EndOfInput
  | MatchFailure Pat
  | CustomError e
  | Boom String
  | Empty
  deriving stock (Eq, Show)

newtype Gen :: Lang -> GenComponent -> Type -> Type -> Type where
  P :: forall (l :: Lang) (c :: GenComponent) (e :: Type) (a :: Type)
     . {runGen :: Pat -> Either [Error e] (a, Pat)}
    -> Gen l c e a

gen ::  Gen l c e (DSL l) -> Pat -> Either [Error e] (DSL l)
gen p pat = case runGen p pat of
  Left errs -> Left errs
  Right (x,_) -> pure x

gen' :: forall (l :: Lang) c e
        . ( DSL l ~ Doc ())
       =>Gen l c e (DSL l) -> Pat -> Either [Error e] (Doc ())
gen' = gen

instance Functor (Gen l c e) where
  fmap f (P p) = P $ \inp -> case p inp of
    Left err -> Left err
    Right (out,rest) -> Right (f out, rest)

instance Applicative (Gen l c e) where
  pure a = P $ \inp -> Right (a,inp)

  P f <*> P p = P $ \inp -> case f inp of
    Left err -> Left err
    Right (f',rest) -> case p rest of
      Left err -> Left err
      Right (out,rest') -> Right (f' out, rest')

instance Monad (Gen l c e) where
  return = pure

  (P p) >>= k = P $ \inp ->
    case p inp of
      Left err -> Left err
      Right (out,rest) ->
        let P p'= k out
        in p' rest

instance Eq e => Alternative (Gen l c e) where
  empty = P $ \_ -> Left [Empty]

  P l <|> P r = P $ \inp ->
    case l inp of
      Left err -> case r inp of
        Left err' -> Left $ nub $ err <> err'
        Right (out,rest) -> Right (out,rest)
      Right (out,rest) -> Right (out,rest)

-- on one hand this is bad. on the other hand, it is good
instance Eq e => MonadFail (Gen l c e) where
  fail _ = empty

type InstanceGen l = Gen l InstanceDecl () (DSL l)

someP :: Eq e => Gen l c e a -> Gen l c e [a]
someP p = do
   x :* xs <- match (_x :* _xs)
   x'  <- result p x
   xs' <- result (manyP p) xs
   pure $ x' : xs'

manyP :: Eq e => Gen l c e a -> Gen l c e [a]
manyP p = match _x >>= \case
  (x :* xs) -> do
    x'  <- result p x
    xs' <- result (manyP p) xs
    pure $ x' : xs'
  Nil -> pure []
  _ -> fail "Expected a pattern list"

matchfail :: Pat -> Either [Error e] a
matchfail =  Left .  pure . MatchFailure

only :: a -> Either [Error e] (a,Pat)
only a = pure (a,Nil)

unsafeToList :: Pat -> [Pat]
unsafeToList = \case
  (x :* xs) -> x : unsafeToList xs
  Nil       -> []
  other     -> error ("not a pattern list " <> show other)

match :: Pat ->  Gen l c e Pat
match targ = P $ \inp ->
  if matches targ inp
  then pure (inp,Nil)
  else Left [MatchFailure inp]

choice :: Eq e => [Gen l c e a] -> Gen l c e a
choice = foldl' (<|>) (fail "No matching TyArg generators!")

result :: Gen l c e a -> Pat -> Gen l c e a
result p pat = P $ \_ -> runGen p pat
