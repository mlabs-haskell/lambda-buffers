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
      `traverse` (plutusFromToGoldenTests <> preludeFromToGoldenTests)

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
  , toFromTest
      "Foo.FInt"
      Correct.genFInt
  , toFromTest
      "Foo.GInt"
      Correct.genGInt
  ]

fooFromToGoldenTests :: [IO TestTree]
fooFromToGoldenTests =
  [ fromToGoldenTest "Foo.A" Golden.aGoldens
  , fromToGoldenTest "Foo.B" Golden.bGoldens
  , fromToGoldenTest "Foo.C" Golden.cGoldens
  , fromToGoldenTest "Foo.D" Golden.dGoldens
  , fromToGoldenTest "Foo.FInt" Golden.fIntGoldens
  , fromToGoldenTest "Foo.GInt" Golden.gIntGoldens
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
  , fromToGoldenTest "PlutusV1.TxInInfo" Golden.txInInfoGoldensV1
  , fromToGoldenTest "PlutusV1.TxOut" Golden.txOutGoldensV1
  , fromToGoldenTest "PlutusV1.DCert" Golden.dCertGoldens
  , fromToGoldenTest "PlutusV1.ScriptPurpose" Golden.scriptPurposeGoldens
  , fromToGoldenTest "PlutusV1.TxInfo" Golden.txInfoGoldensV1
  , fromToGoldenTest "PlutusV1.ScriptContext" Golden.scriptContextGoldensV1
  , fromToGoldenTest "PlutusV2.TxInInfo" Golden.txInInfoGoldensV2
  , fromToGoldenTest "PlutusV2.OutputDatum" Golden.outDatumGoldens
  , fromToGoldenTest "PlutusV2.TxOut" Golden.txOutGoldensV2
  , fromToGoldenTest "PlutusV2.TxInfo" Golden.txInfoGoldensV2
  , fromToGoldenTest "PlutusV2.ScriptContext" Golden.scriptContextGoldensV2
  , fromToGoldenTest "PlutusV3.Rational" Golden.rationalGoldens
  , fromToGoldenTest "PlutusV3.TxId" Golden.txIdGoldensV3
  , fromToGoldenTest "PlutusV3.TxOutRef" Golden.txOutRefGoldensV3
  , fromToGoldenTest "PlutusV3.ColdCommitteeCredential" Golden.coldCommitteeCredentialGoldensV3
  , fromToGoldenTest "PlutusV3.HotCommitteeCredential" Golden.hotCommitteeCredentialGoldensV3
  , fromToGoldenTest "PlutusV3.DRepCredential" Golden.drepCredentialGoldensV3
  , fromToGoldenTest "PlutusV3.DRep" Golden.drepGoldensV3
  , fromToGoldenTest "PlutusV3.Delegatee" Golden.delegateeGoldensV3
  , fromToGoldenTest "PlutusV1.Lovelace" Golden.lovelaceGoldens
  , fromToGoldenTest "PlutusV3.TxCert" Golden.txCertGoldensV3
  , fromToGoldenTest "PlutusV3.Voter" Golden.voterGoldensV3
  , fromToGoldenTest "PlutusV3.Vote" Golden.voteGoldensV3
  , fromToGoldenTest "PlutusV3.GovernanceActionId" Golden.governanceActionIdGoldensV3
  , fromToGoldenTest "PlutusV3.Committee" Golden.committeeGoldensV3
  , fromToGoldenTest "PlutusV3.Constitution" Golden.constitutionGoldensV3
  , fromToGoldenTest "PlutusV3.ProtocolVersion" Golden.protocolVersionGoldensV3
  , fromToGoldenTest "PlutusV3.ChangedParameters" Golden.changedParametersGoldensV3
  , fromToGoldenTest "PlutusV3.GovernanceAction" Golden.governanceActionGoldensV3
  , fromToGoldenTest "PlutusV3.ProposalProcedure" Golden.proposalProcedureGoldensV3
  , fromToGoldenTest "PlutusV3.ScriptPurpose" Golden.scriptPurposeGoldensV3
  , fromToGoldenTest "PlutusV3.ScriptInfo" Golden.scriptInfoGoldensV3
  , fromToGoldenTest "PlutusV3.TxInInfo" Golden.txInInfoGoldensV3
  , fromToGoldenTest "PlutusV3.TxInfo" Golden.txInfoGoldensV3
  , fromToGoldenTest "PlutusV3.ScriptContext" Golden.scriptContextGoldensV3
  ]

-- | Prelude tests.
preludeFromToGoldenTests :: [IO TestTree]
preludeFromToGoldenTests =
  [ fromToGoldenTest "Prelude.Bool" Golden.boolGoldens
  , fromToGoldenTest "Prelude.Maybe" Golden.maybeGoldens
  , fromToGoldenTest "Prelude.Either" Golden.eitherGoldens
  , fromToGoldenTest "Prelude.List" Golden.listGoldens
  ]
