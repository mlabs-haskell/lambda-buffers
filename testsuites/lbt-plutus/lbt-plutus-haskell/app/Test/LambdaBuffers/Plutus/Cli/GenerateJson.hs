module Test.LambdaBuffers.Plutus.Cli.GenerateJson (GenerateJsonOpts (..), generateJson) where

import Data.Foldable (for_)
import LambdaBuffers.Runtime.Plutus ()
import Test.LambdaBuffers.Plutus.Golden qualified as Golden
import Test.LambdaBuffers.Plutus.Golden.Json qualified as GoldenJson

data GenerateJsonOpts = GenerateJsonOpts {maxSamples :: Int, directory :: FilePath} deriving stock (Show, Eq, Ord)

generateJson :: GenerateJsonOpts -> IO ()
generateJson opts = do
  let goldenDir = directory opts
      n = maxSamples opts
  fps <-
    mconcat
      [ GoldenJson.writeGoldens goldenDir "PlutusV1.PlutusData" $ take n Golden.plutusDataGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Address" $ take n Golden.addressGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Credential" $ take n Golden.credentialGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.StakingCredential" $ take n Golden.stakingCredentialGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.PubKeyHash" $ take n Golden.pubKeyHashGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Bytes" $ take n Golden.bytesGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Interval" $ take n Golden.intervalGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Extended" $ take n Golden.extendedGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.LowerBound" $ take n Golden.lowerBoundGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.UpperBound" $ take n Golden.upperBoundGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.POSIXTime" $ take n Golden.posixTimeGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.POSIXTimeRange" $ take n Golden.posixTimeRangeGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.CurrencySymbol" $ take n (Golden.adaCurrencySymbolGolden : Golden.currencySymbolGoldens)
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TokenName" $ take n Golden.tokenNameGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.AssetClass" $ take n Golden.assetClassGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Value" $ take n Golden.valueGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Redeemer" $ take n Golden.redeemerGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Datum" $ take n Golden.datumGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.RedeemerHash" $ take n Golden.redeemerHashGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.DatumHash" $ take n Golden.datumHashGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.ScriptHash" $ take n Golden.scriptHashGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TxId" $ take n Golden.txIdGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TxOutRef" $ take n Golden.txOutRefGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Map" $ take n Golden.mapGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TxInInfo" $ take n Golden.txInInfoGoldensV1
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TxOut" $ take n Golden.txOutGoldensV1
      , GoldenJson.writeGoldens goldenDir "PlutusV1.DCert" $ take n Golden.dCertGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.ScriptPurpose" $ take n Golden.scriptPurposeGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV1.TxInfo" $ take n Golden.txInfoGoldensV1
      , GoldenJson.writeGoldens goldenDir "PlutusV1.ScriptContext" $ take n Golden.scriptContextGoldensV1
      , GoldenJson.writeGoldens goldenDir "PlutusV2.TxInInfo" $ take n Golden.txInInfoGoldensV2
      , GoldenJson.writeGoldens goldenDir "PlutusV2.OutputDatum" $ take n Golden.outDatumGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV2.TxOut" $ take n Golden.txOutGoldensV2
      , GoldenJson.writeGoldens goldenDir "PlutusV2.TxInfo" $ take n Golden.txInfoGoldensV2
      , GoldenJson.writeGoldens goldenDir "PlutusV2.ScriptContext" $ take n Golden.scriptContextGoldensV2
      ]
  putStrLn "[lbt-plutus-golden] Wrote Json goldens:"
  for_ fps putStrLn
