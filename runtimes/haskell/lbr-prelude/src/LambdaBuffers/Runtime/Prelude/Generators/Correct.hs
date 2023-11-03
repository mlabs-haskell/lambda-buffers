module LambdaBuffers.Runtime.Prelude.Generators.Correct (genBool, genInteger, genChar, genText, genMaybe, genEither, genBytes, genList, genSet, genMap, genComplicated) where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR

genBool :: H.Gen Bool
genBool = H.bool

genInteger :: H.Gen Integer
genInteger = H.integral (HR.constant (-100000000000000000000000000000000000000000000000) 100000000000000000000000000000000000000000000000)

genChar :: H.Gen Char
genChar = H.unicode -- WARN(bladyjoker): Using H.unicodeAll breaks the tests \65533 != \55296

genBytes :: H.Gen ByteString
genBytes = H.bytes (HR.constant 0 500)

genText :: H.Gen Text
genText = H.text (HR.constant 0 500) genChar

genMaybe :: H.Gen a -> H.Gen (Maybe a)
genMaybe gx = H.choice [Just <$> gx, return Nothing]

genEither :: H.Gen a -> H.Gen b -> H.Gen (Either a b)
genEither gl gr = H.choice [Left <$> gl, Right <$> gr]

genList :: H.Gen a -> H.Gen [a]
genList = H.list (HR.constant 0 10)

genSet :: Ord a => H.Gen a -> H.Gen (Set a)
genSet = H.set (HR.constant 0 10)

genMap :: (Ord k) => H.Gen k -> H.Gen v -> H.Gen (Map k v)
genMap gk gv =
  Map.fromList <$> do
    keys <- List.nub <$> H.list (HR.constant 0 10) gk
    (\k -> (k,) <$> gv) `traverse` keys

genComplicated ::
  H.Gen
    (Map Text (Either (Maybe (Either Bool ByteString)) [Set Char]))
genComplicated =
  genMap
    genText
    ( genEither
        (genMaybe (genEither genBool genBytes))
        (genList (genSet genChar))
    )
