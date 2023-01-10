{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds, PolyKinds, GADTs, RankNTypes, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Gen.Generator where

import Data.List ( nub, foldl' )
import Common.Match
import Common.Types
import Control.Applicative (Alternative(..))
import Data.Kind
import Prettyprinter (Doc)

data Lang where
  Rust :: Lang
  PureScript :: Lang
  Haskell :: Lang
  Plutarch :: Lang
  deriving (Show, Eq)

data GenComponent = TypeDecl | InstanceDecl

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
  deriving (Eq, Show)

newtype Parser :: Lang -> GenComponent -> Type -> Type -> Type where
  P :: forall (l :: Lang) (c :: GenComponent) (e :: Type) (a :: Type)
     . {runParser :: Pat -> Either [Error e] (a, Pat)}
    -> Parser l c e a

parse ::  Parser l c e (DSL l) -> Pat -> Either [Error e] (DSL l)
parse p pat = case runParser p pat of
  Left errs -> Left errs
  Right (x,_) -> pure x

parse' :: forall (l :: Lang) c e
        . (TargetLang l, DSL l ~ Doc ())
       => Parser l c e (DSL l) -> Pat -> Either [Error e] (Doc ())
parse' = parse

instance Functor (Parser l c e) where
  fmap f (P p) = P $ \inp -> case p inp of
    Left err -> Left err
    Right (out,rest) -> Right (f out, rest)

instance Applicative (Parser l c e) where
  pure a = P $ \inp -> Right (a,inp)

  P f <*> P p = P $ \inp -> case f inp of
    Left err -> Left err
    Right (f',rest) -> case p rest of
      Left err -> Left err
      Right (out,rest') -> Right (f' out, rest')

instance Monad (Parser l c e) where
  return = pure

  (P p) >>= k = P $ \inp ->
    case p inp of
      Left err -> Left err
      Right (out,rest) ->
        let P p'= k out
        in p' rest

instance Eq e => Alternative (Parser l c e) where
  empty = P $ \_ -> Left [Empty]

  P l <|> P r = P $ \inp ->
    case l inp of
      Left err -> case r inp of
        Left err' -> Left $ nub $ err <> err'
        Right (out,rest) -> Right (out,rest)
      Right (out,rest) -> Right (out,rest)

-- on one hand this is bad. on the other hand, it is good
instance Eq e => MonadFail (Parser l c e) where
  fail _ = empty

_k, _v, _a, _l, _x, _xs, _name, _vars, _nil, _body :: Pat
_k    = VarP "kdasfadsfsadf3323232421413413413"
_v    = VarP "vdsfasdfa3e4fewafewufioeoifioaefiowe"
_a    = VarP "a3242432aefiosdjfioasdf32jior3j2oirj32io"
_l    = VarP "lasdfsdfj3ij319031j8381913j8138"
_x    = VarP "x32fjio23io32jio3j2iof3ijo2fio32fio32"
_xs   = VarP "xsadsfjdlsfji3i3298238893289j32j8923j89"
_name = VarP "namedsafdsfhuisdhfuidshu3893283h8df398hdf321"
_body = VarP "bodyadfjasidofjads08f8943j289234j89234j98fj38"
_vars = VarP "varsdsfdasfjklsdafjilsdafjiasdio43io2903"
_nil  = VarP "nildsfjosdjfiosdajfoi89321893y8914981398"


someP :: Eq e => Parser l c e a -> Parser l c e [a]
someP p = do
   x :* xs <- match (_x :* _xs)
   x'  <- result p x
   xs' <- result (manyP p) xs
   pure $ x' : xs'

manyP :: Eq e => Parser l c e a -> Parser l c e [a]
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

match :: Pat ->  Parser l c e Pat
match targ = P $ \inp ->
  if matches targ inp
  then pure (inp,Nil)
  else Left [MatchFailure inp]

choice :: Eq e => [Parser l c e a] -> Parser l c e a
choice = foldl' (<|>) (fail "No matching TyArg generators!")

result :: Parser l c e a -> Pat -> Parser l c e a
result p pat = P $ \_ -> runParser p pat
