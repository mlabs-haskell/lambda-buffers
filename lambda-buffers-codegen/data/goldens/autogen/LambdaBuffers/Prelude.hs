module LambdaBuffers.Prelude (Bool(..)
                             , Bytes(..)
                             , Char(..)
                             , Either(..)
                             , Int16(..)
                             , Int32(..)
                             , Int64(..)
                             , Int8(..)
                             , Integer(..)
                             , List(..)
                             , Map(..)
                             , Maybe(..)
                             , Set(..)
                             , Text(..)
                             , UInt16(..)
                             , UInt32(..)
                             , UInt64(..)
                             , UInt8(..)) where

import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Word
import qualified LambdaBuffers.Runtime.Haskell
import qualified Prelude

type Bool = Prelude.Bool
type Bytes = Data.ByteString.ByteString
type Char = Prelude.Char
type Either a b = Prelude.Either a b
type Int16 = Data.Int.Int16
type Int32 = Data.Int.Int32
type Int64 = Data.Int.Int64
type Int8 = Data.Int.Int8
type Integer = Prelude.Integer
type List a = LambdaBuffers.Runtime.Haskell.List a
type Map a b = Data.Map.Map a b
type Maybe a = Prelude.Maybe a
type Set a = Data.Set.Set a
type Text = Data.Text.Text
type UInt16 = Data.Word.Word16
type UInt32 = Data.Word.Word32
type UInt64 = Data.Word.Word64
type UInt8 = Data.Word.Word8

