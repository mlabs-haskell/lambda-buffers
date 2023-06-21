module Test.LambdaBuffers.Plutus.Cli.GeneratePlutusData (GeneratePlutusDataOpts (..), generatePlutusData) where

import Data.Foldable (for_)
import LambdaBuffers.Runtime.Plutus ()
import Test.LambdaBuffers.Plutus.Golden qualified as Golden
import Test.LambdaBuffers.Plutus.Golden.PlutusData qualified as GoldenPlutusData

data GeneratePlutusDataOpts = GeneratePlutusDataOpts {maxSamples :: Int, directory :: FilePath} deriving stock (Show, Eq, Ord)

generatePlutusData :: GeneratePlutusDataOpts -> IO ()
generatePlutusData opts = do
  let goldenDir = directory opts
      n = maxSamples opts
  fps <-
    mconcat
      [ GoldenPlutusData.writeGoldens goldenDir "PlutusV1.PlutusData" Golden.plutusDataGoldens'
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Address" $ take n Golden.addressGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Credential" $ take n Golden.credentialGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.StakingCredential" $ take n Golden.stakingCredentialGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.PubKeyHash" $ take n Golden.pubKeyHashGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Bytes" $ take n Golden.bytesGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Interval" $ take n Golden.intervalGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Extended" $ take n Golden.extendedGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.LowerBound" $ take n Golden.lowerBoundGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.UpperBound" $ take n Golden.upperBoundGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.POSIXTime" $ take n Golden.posixTimeGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.POSIXTimeRange" $ take n Golden.posixTimeRangeGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.CurrencySymbol" $ take n (Golden.adaCurrencySymbolGolden : Golden.currencySymbolGoldens)
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TokenName" $ take n Golden.tokenNameGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.AssetClass" $ take n Golden.assetClassGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Value" $ take n Golden.valueGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Redeemer" $ take n Golden.redeemerGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Datum" $ take n Golden.datumGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.RedeemerHash" $ take n Golden.redeemerHashGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.DatumHash" $ take n Golden.datumHashGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.ScriptHash" $ take n Golden.scriptHashGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TxId" $ take n Golden.txIdGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TxOutRef" $ take n Golden.txOutRefGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Map" $ take n Golden.mapGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.TxInInfo" $ take n Golden.txInInfoGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.OutputDatum" $ take n Golden.outDatumGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.TxOut" $ take n Golden.txOutGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Days.Day" $ take n Golden.dayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Days.WorkDay" $ take n Golden.workDayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Days.FreeDay" $ take n Golden.freeDayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.A" $ take n Golden.aGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.B" $ take n Golden.bGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.C" $ take n Golden.cGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.D" $ take n Golden.dGoldens
      ]
  putStrLn "[lbt-plutus-golden] Wrote PlutusData goldens:"
  for_ fps putStrLn
