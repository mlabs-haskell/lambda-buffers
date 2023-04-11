module LambdaBuffers.Prelude (Bool(..)
                             , Bytes(..)
                             , Char(..)
                             , Either(..)
                             , Int32(..)
                             , Integer(..)
                             , List(..)
                             , Map(..)
                             , Maybe(..)
                             , Set(..)
                             , Text(..)) where

import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified LambdaBuffers.Runtime.Haskell
import qualified Prelude


type Bool = Prelude.Bool

type Bytes = Data.ByteString.ByteString

type Char = Prelude.Char

type Either a b = Prelude.Either a b

type Int32 = Data.Int.Int32

type Integer = Prelude.Integer

type List a = LambdaBuffers.Runtime.Haskell.List a

type Map a b = Data.Map.Map a b

type Maybe a = Prelude.Maybe a

type Set a = Data.Set.Set a

type Text = Data.Text.Text

