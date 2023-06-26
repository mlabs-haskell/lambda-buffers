-- | This module exports a newtype wrapper for Uint8Array solely because the original type lacks Show and Eq instances.
module LambdaBuffers.Runtime.Prelude.Bytes
  ( Bytes(..)
  , fromIntArray
  , toIntArray
  ) where

import Prelude
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype, unwrap)
import Data.UInt as UInt
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Prelude.Json (class Json, toJson)

newtype Bytes
  = Bytes Uint8Array

derive instance ntBytes :: Newtype Bytes _

instance showBytes :: Show Bytes where
  show = unwrap >>> toJson >>> show

instance eqBytes :: Eq Bytes where
  eq l r = unsafePerformEffect $ ArrayBuffer.eq (unwrap l) (unwrap r)

derive newtype instance jsonBytes :: Json Bytes

-- | Some utility functions.
fromIntArray :: Array Int -> Bytes
fromIntArray xs = Bytes $ unsafePerformEffect (ArrayBuffer.fromArray (UInt.fromInt <$> xs))

toIntArray :: Bytes -> Array Int
toIntArray b = UInt.toInt <$> unsafePerformEffect (ArrayBuffer.toArray (unwrap b))
