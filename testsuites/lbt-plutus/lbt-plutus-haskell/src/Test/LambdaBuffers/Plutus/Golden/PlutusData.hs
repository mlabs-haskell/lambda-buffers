module Test.LambdaBuffers.Plutus.Golden.PlutusData (writeGoldens, fromToGoldenTest) where

import Data.ByteString qualified as B
import LambdaBuffers.Runtime.Plutus ()
import LambdaBuffers.Runtime.Prelude (fromJsonBytes, toJsonBytes)
import PlutusTx qualified
import Test.LambdaBuffers.Plutus.Golden.Utils qualified as Utils
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertEqual, assertFailure)

writeGoldens :: PlutusTx.ToData a => FilePath -> TestName -> [a] -> IO [FilePath]
writeGoldens goldenDir title goldens = Utils.writeGoldens goldenDir title ".pd.json" (PlutusTx.toData <$> goldens)

-- | `fromToGoldenTest goldenDir title goldens`
fromToGoldenTest :: forall {a}. (Eq a, Show a, PlutusTx.FromData a, PlutusTx.ToData a) => FilePath -> TestName -> [a] -> IO TestTree
fromToGoldenTest goldenDir title =
  Utils.assertGoldens
    goldenDir
    title
    ".pd.json"
    (\x -> "(toJson . toPlutusData . fromPlutusData . fromJson) " <> x <> " == " <> x)
    ( \golden index fp -> do
        pdJson <- B.readFile fp
        case fromJsonBytes @PlutusTx.Data pdJson of
          Left err -> assertFailure $ show ("Failed parsing PlutusData from Json" :: String, title, index, fp, err)
          Right pd -> do
            case PlutusTx.fromData @a pd of
              Nothing -> assertFailure $ show ("Failed parsing PlutusData" :: String, title, index, fp)
              Just res -> do
                assertEqual "Golden values should match" golden res
                assertEqual "Golden bytes should match" pdJson (toJsonBytes . PlutusTx.toData $ res)
    )
