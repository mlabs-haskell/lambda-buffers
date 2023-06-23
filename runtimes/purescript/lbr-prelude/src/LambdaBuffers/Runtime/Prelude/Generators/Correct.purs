module LambdaBuffers.Runtime.Prelude.Generators.Correct (genBool, genInteger, genChar, genText, genMaybe, genEither, genList, genSet, genMap, genBytes) where

import Prelude
import Control.Monad.Gen.Common as MGC
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Typed.Gen (genTypedArray, genUint8) as ArrayBuffer
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import LambdaBuffers.Runtime.Prelude.Bytes (Bytes(..))
import Test.QuickCheck (arbitrary) as Q
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt) as Q
import Test.QuickCheck.UTF8String as UTF8String

genBool :: Q.Gen Boolean
genBool = Q.arbitrary

genInteger :: Q.Gen BigInt
genInteger = BigInt.fromInt <$> Q.chooseInt (-100000) 100000

genChar :: Q.Gen Char
genChar = UTF8String.genChar

genBytes :: Q.Gen Bytes
genBytes = Bytes <$> ArrayBuffer.genTypedArray ArrayBuffer.genUint8

genText :: Q.Gen String
genText = UTF8String.genString

genMaybe :: Q.Gen (Maybe String)
genMaybe = MGC.genMaybe genText

genEither :: Q.Gen (Either String String)
genEither = MGC.genEither genText genText

genList :: Q.Gen (Array String)
genList = Q.arrayOf genText

genSet :: Q.Gen (Set String)
genSet = Set.fromFoldable <$> genList

genMap :: Q.Gen (Map String String)
genMap = do
  keys <- genSet
  kvs <- (\k -> genText >>= \v -> pure $ Tuple k v) `traverse` fromFoldable keys
  pure $ Map.fromFoldable kvs

-- TODO(bladyjoker): It's tiring to work with NonEmpty
-- genComplicated ::
--   H.Gen
--     (Map Text (Either (Maybe (Either Bool ByteString)) [Set Char]))
-- genComplicated =
--   genMap
--     genText
--     ( genEither
--         (genMaybe (genEither genBool genBytes))
--         (genList (genSet genChar))
--     )
