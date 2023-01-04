{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.Generator where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.List (nub)
import Common.Match
import Common.Types
import Control.Applicative (Alternative(..))

-- adapted from https://serokell.io/blog/parser-combinators-in-haskell
data Error e
  = EndOfInput
  | Unexpected Pat
  | CustomError e
  | Boom String
  | Empty
  deriving (Eq, Show)

newtype Parser e a = P
  { runParser :: Pat -> Either [Error e] (a, Pat)
  }

parse :: Parser e a -> Pat -> Either [Error e] a
parse p pat = case runParser p pat of
  Left errs -> Left errs
  Right (x,_) -> pure x

instance Functor (Parser e) where
  fmap f (P p) = P $ \inp -> case p inp of
    Left err -> Left err
    Right (out,rest) -> Right (f out, rest)

instance Applicative (Parser e) where
  pure a = P $ \inp -> Right (a,inp)

  P f <*> P p = P $ \inp -> case f inp of
    Left err -> Left err
    Right (f',rest) -> case p rest of
      Left err -> Left err
      Right (out,rest') -> Right (f' out, rest')

instance Monad (Parser e) where
  return = pure

  (P p) >>= k = P $ \inp ->
    case p inp of
      Left err -> Left err
      Right (out,rest) ->
        let P p'= k out
        in p' rest

instance Eq e => Alternative (Parser e) where
  empty = P $ \_ -> Left [Empty]

  P l <|> P r = P $ \inp ->
    case l inp of
      Left err -> case r inp of
        Left err' -> Left $ nub $ err <> err'
        Right (out,rest) -> Right (out,rest)
      Right (out,rest) -> Right (out,rest)

-- on one hand this is bad. on the other hand, it is good
instance Eq e => MonadFail (Parser e) where
  fail _ = empty

_k, _v, _a, _l, _x, _xs, _name, _vars, _nil :: Pat
_k    = VarP "kdasfadsfsadf3323232421413413413"
_v    = VarP "vdsfasdfa3e4fewafewufioeoifioaefiowe"
_a    = VarP "a3242432aefiosdjfioasdf32jior3j2oirj32io"
_l    = VarP "lasdfsdfj3ij319031j8381913j8138"
_x    = VarP "x32fjio23io32jio3j2iof3ijo2fio32fio32"
_xs   = VarP "xsadsfjdlsfji3i3298238893289j32j8923j89"
_name = VarP "namedsafdsfhuisdhfuidshu3893283h8df398hdf321"
_vars = VarP "varsdsfdasfjklsdafjilsdafjiasdio43io2903"
_nil  = VarP "nildsfjosdjfiosdajfoi89321893y8914981398"


someP :: Eq e => Parser e a -> Parser e [a]
someP p = do
   x :* xs <- match (_x :* _xs)
   x'  <- result p x
   xs' <- result (manyP p) xs
   pure $ x' : xs'

manyP :: Eq e => Parser e a -> Parser e [a]
manyP p = match _x >>= \case
  (x :* xs) -> do
    x'  <- result p x
    xs' <- result (manyP p) xs
    pure $ x' : xs'
  Nil -> pure []
  other -> fail "Expected a pattern list"

unexpected :: Pat -> Either [Error e] a
unexpected =  Left .  pure . Unexpected

only :: a -> Either [Error e] (a,Pat)
only a = pure (a,Nil)

match :: Pat ->  Parser e Pat
match targ = P $ \inp ->
  if matches targ inp
  then pure (inp,Nil)
  else Left [Unexpected inp]

result :: Parser e a -> Pat -> Parser e a
result p pat = P $ \_ -> runParser p pat
