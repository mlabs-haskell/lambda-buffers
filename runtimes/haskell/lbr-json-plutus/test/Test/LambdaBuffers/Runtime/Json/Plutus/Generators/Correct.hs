module Test.LambdaBuffers.Runtime.Json.Plutus.Generators.Correct (genValue, genCurrencySymbol, genTokenName, genAssetClass) where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusTx.AssocMap qualified as PlutusTx

-- | Default constant range used in various generators
defRange :: HR.Range Int
defRange = HR.constant 0 5

genAssetClass :: H.Gen PlutusV1.AssetClass
genAssetClass = PlutusV1.AssetClass <$> genPair genCurrencySymbol genTokenName

genCurrencySymbol :: H.Gen PlutusV1.CurrencySymbol
genCurrencySymbol = PlutusV1.CurrencySymbol <$> genPlutusBytes 28

genTokenName :: H.Gen PlutusV1.TokenName
genTokenName = PlutusV1.TokenName <$> genPlutusBytes' 32

genAmount :: H.Gen Integer
genAmount = H.integral (HR.constant (-100) 100)

genValue :: H.Gen PlutusV1.Value
genValue = PlutusV1.Value <$> genMap genCurrencySymbol (genMap genTokenName genAmount)

genMap :: (Eq k) => H.Gen k -> H.Gen v -> H.Gen (PlutusTx.Map k v)
genMap gk gv =
  PlutusTx.fromList <$> do
    keys <- List.nub <$> H.list defRange gk
    (\k -> (k,) <$> gv) `traverse` keys

genPair :: H.Gen x -> H.Gen y -> H.Gen (x, y)
genPair x y = (,) <$> x <*> y

genPlutusBytes :: Int -> H.Gen PlutusV1.BuiltinByteString
genPlutusBytes len = PlutusV1.toBuiltin <$> genBytes len

genBytes :: Int -> H.Gen ByteString
genBytes len = H.bytes (HR.singleton len)

genPlutusBytes' :: Int -> H.Gen PlutusV1.BuiltinByteString
genPlutusBytes' maxLen = PlutusV1.toBuiltin <$> genBytes' maxLen

genBytes' :: Int -> H.Gen ByteString
genBytes' maxLen = H.bytes (HR.constant 0 maxLen)
