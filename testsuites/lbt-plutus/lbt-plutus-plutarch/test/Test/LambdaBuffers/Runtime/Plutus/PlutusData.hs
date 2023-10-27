module Test.LambdaBuffers.Runtime.Plutus.PlutusData (tests) where

import Hedgehog qualified as H
import Paths_lbt_plutus_golden_data qualified as Paths
import PlutusTx (FromData, ToData, fromData, toData)
import Test.LambdaBuffers.Plutus.Golden qualified as Golden
import Test.LambdaBuffers.Plutus.Golden.PlutusData qualified as Golden
import Test.LambdaBuffers.Runtime.Plutus.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

tests :: IO TestTree
tests = do
  goldenDerived <- goldenDerivedTests
  goldenInstance <- goldenInstanceTests
  return $
    testGroup
      "Plutus.V1.PlutusData class tests"
      [ testGroup "Derive" [goldenDerived, propsDerived]
      , testGroup "Instance" [goldenInstance]
      ]

propsDerived :: TestTree
propsDerived =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Property tests"
      ( fooToFromTests
          <> daysToFromTests
      )

goldenDerivedTests :: IO TestTree
goldenDerivedTests = do
  gts <-
    id
      `traverse` (daysFromToGoldenTests <> fooFromToGoldenTests)

  return $
    testGroup
      "Golden tests"
      gts

goldenInstanceTests :: IO TestTree
goldenInstanceTests = do
  gts <-
    id
      `traverse` plutusFromToGoldenTests

  return $
    testGroup
      "Golden tests"
      gts

toFromTest :: forall {a}. (Show a, Eq a, ToData a, FromData a) => TestName -> H.Gen a -> TestTree
toFromTest title gen =
  testProperty
    ("forall (x : " <> title <> "): (fromPlutusData . toPlutusData) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromData . toData) x H.=== Just x
    )

fromToGoldenTest :: forall {a}. (ToData a, FromData a, Eq a, Show a) => TestName -> [a] -> IO TestTree
fromToGoldenTest title goldens = do
  goldenDir <- Paths.getDataFileName "data"
  Golden.fromToGoldenTest goldenDir title goldens

-- | Foo
fooToFromTests :: [TestTree]
fooToFromTests =
  [ toFromTest
      "Foo.A"
      Correct.genA
  , toFromTest
      "Foo.B"
      Correct.genB
  , toFromTest
      "Foo.C"
      Correct.genC
  , toFromTest
      "Foo.D"
      Correct.genD
  ]

fooFromToGoldenTests :: [IO TestTree]
fooFromToGoldenTests =
  [ fromToGoldenTest "Foo.A" Golden.aGoldens
  , fromToGoldenTest "Foo.B" Golden.bGoldens
  , fromToGoldenTest "Foo.C" Golden.cGoldens
  , fromToGoldenTest "Foo.D" Golden.dGoldens
  ]

-- | Days
daysToFromTests :: [TestTree]
daysToFromTests =
  [ toFromTest
      "Days.Day"
      Correct.genDay
  , toFromTest
      "Days.WorkDay"
      Correct.genWorkDay
  , toFromTest
      "Days.FreeDay"
      Correct.genFreeDay
  ]

daysFromToGoldenTests :: [IO TestTree]
daysFromToGoldenTests =
  [ fromToGoldenTest "Days.Day" Golden.dayGoldens
  , fromToGoldenTest "Days.WorkDay" Golden.workDayGoldens
  , fromToGoldenTest "Days.FreeDay" Golden.freeDayGoldens
  ]

-- | Plutus.V1
plutusFromToGoldenTests :: [IO TestTree]
plutusFromToGoldenTests =
  [ fromToGoldenTest "PlutusV1.PlutusData" Golden.plutusDataGoldens'
  , fromToGoldenTest "PlutusV1.Address" Golden.addressGoldens
  , fromToGoldenTest "PlutusV1.Credential" Golden.credentialGoldens
  , fromToGoldenTest "PlutusV1.StakingCredential" Golden.stakingCredentialGoldens
  , fromToGoldenTest "PlutusV1.PubKeyHash" Golden.pubKeyHashGoldens
  , fromToGoldenTest "PlutusV1.Bytes" Golden.bytesGoldens
  , fromToGoldenTest "PlutusV1.Interval" Golden.intervalGoldens
  , fromToGoldenTest "PlutusV1.Extended" Golden.extendedGoldens
  , fromToGoldenTest "PlutusV1.LowerBound" Golden.lowerBoundGoldens
  , fromToGoldenTest "PlutusV1.UpperBound" Golden.upperBoundGoldens
  , fromToGoldenTest "PlutusV1.POSIXTime" Golden.posixTimeGoldens
  , fromToGoldenTest "PlutusV1.POSIXTimeRange" Golden.posixTimeRangeGoldens
  , fromToGoldenTest "PlutusV1.CurrencySymbol" (Golden.adaCurrencySymbolGolden : Golden.currencySymbolGoldens)
  , fromToGoldenTest "PlutusV1.TokenName" Golden.tokenNameGoldens
  , fromToGoldenTest "PlutusV1.AssetClass" Golden.assetClassGoldens
  , fromToGoldenTest "PlutusV1.Value" Golden.valueGoldens
  , fromToGoldenTest "PlutusV1.Redeemer" Golden.redeemerGoldens
  , fromToGoldenTest "PlutusV1.Datum" Golden.datumGoldens
  , fromToGoldenTest "PlutusV1.RedeemerHash" Golden.redeemerHashGoldens
  , fromToGoldenTest "PlutusV1.DatumHash" Golden.datumHashGoldens
  , fromToGoldenTest "PlutusV1.ScriptHash" Golden.scriptHashGoldens
  , fromToGoldenTest "PlutusV1.TxId" Golden.txIdGoldens
  , fromToGoldenTest "PlutusV1.TxOutRef" Golden.txOutRefGoldens
  , fromToGoldenTest "PlutusV1.Map" Golden.mapGoldens
  , fromToGoldenTest "PlutusV2.TxInInfo" Golden.txInInfoGoldens
  , fromToGoldenTest "PlutusV2.OutputDatum" Golden.outDatumGoldens
  , fromToGoldenTest "PlutusV2.TxOut" Golden.txOutGoldens
  ]
