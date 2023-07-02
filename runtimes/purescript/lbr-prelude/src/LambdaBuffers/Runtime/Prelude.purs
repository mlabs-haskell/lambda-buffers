module LambdaBuffers.Runtime.Prelude
  ( module Json
  , module Bytes
  , caseInt
  ) where

import Data.Array (filter, uncons)
import Data.BigInt (BigInt)
import Data.Eq ((==))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import LambdaBuffers.Runtime.Prelude.Bytes (Bytes(..), fromIntArray, toIntArray) as Bytes
import LambdaBuffers.Runtime.Prelude.Json (class Json, Parser, caseJsonArray, caseJsonConstructor, caseJsonObject, fail, fromJson, fromJsonBytes, fromJsonString, jsonArray, jsonConstructor, jsonField, jsonObject, toJson, toJsonBytes, toJsonString) as Json

-- | LamVal primitive `caseInt`
caseInt :: forall a. Array (Tuple BigInt a) -> (BigInt -> a) -> BigInt -> a
caseInt cases otherCase i = case uncons (filter (\(Tuple i' _) -> i' == i) cases) of
  Just { head: Tuple _ res, tail: _ } -> res
  Nothing -> otherCase i
