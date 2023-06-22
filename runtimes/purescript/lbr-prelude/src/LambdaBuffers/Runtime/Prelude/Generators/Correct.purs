module LambdaBuffers.Runtime.Prelude.Generators.Correct (genBool, genInteger, genChar, genText, genMaybe, genEither, genList, genSet, genMap, genBytes, Bytes(..)) where

import Prelude

import Control.Monad.Gen.Common as MGC
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Typed.Gen (genTypedArray, genUint8) as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Gen (genAsciiString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import LambdaBuffers.Runtime.Prelude (class Json, toJson)
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
genEither = MGC.genEither genAsciiString genAsciiString

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

-- | Needed just for testing
newtype Bytes = Bytes Uint8Array

derive instance Newtype Bytes _

instance Show Bytes where
  show = unwrap >>> toJson >>> show

instance Eq Bytes where
  eq l r = toJson l == toJson r

derive newtype instance Json Bytes
