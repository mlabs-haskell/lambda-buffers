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

import Ctl.Internal.Types.ByteArray as Ctl.Internal.Types.ByteArray
import Data.BigInt as Data.BigInt
import Data.Either as Data.Either
import Data.Map as Data.Map
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Set as Data.Set
import Prelude as Prelude
import Prim as Prim


type Bool = Prim.Boolean


type Bytes = Ctl.Internal.Types.ByteArray.ByteArray


type Char = Prim.Char


type Either a b = Data.Either.Either a b


type Int32 = Prim.Int


type Integer = Data.BigInt.BigInt


type List a = Prim.Array a


type Map a b = Data.Map.Map a b


type Maybe a = Data.Maybe.Maybe a


type Set a = Data.Set.Set a


type Text = Prim.String


