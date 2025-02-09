module Test.LambdaBuffers.Runtime.Plutus.Json (tests) where

import LambdaBuffers.Runtime.Prelude (Json)
import Paths_lbt_plutus_golden_data qualified as Paths
import Test.LambdaBuffers.Plutus.Golden qualified as Golden
import Test.LambdaBuffers.Plutus.Golden.Json qualified as Golden
import Test.Tasty (TestName, TestTree, testGroup)

tests :: IO TestTree
tests = do
  goldenInstance <- goldenInstanceTests
  return $
    testGroup
      "Prelude.Json class tests"
      [ testGroup "Instance" [goldenInstance]
      ]

goldenInstanceTests :: IO TestTree
goldenInstanceTests = do
  gts <-
    id
      `traverse` plutusFromToGoldenTests

  return $
    testGroup
      "Golden tests"
      gts

fromToGoldenTest :: forall {a}. (Json a, Eq a, Show a) => TestName -> [a] -> IO TestTree
fromToGoldenTest title goldens = do
  goldenDir <- Paths.getDataFileName "data"
  Golden.fromToGoldenTest goldenDir title goldens

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
