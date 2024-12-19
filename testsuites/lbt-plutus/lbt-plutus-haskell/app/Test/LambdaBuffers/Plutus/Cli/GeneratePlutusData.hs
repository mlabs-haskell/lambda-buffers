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
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TxInInfo" $ take n Golden.txInInfoGoldensV1
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.DCert" $ take n Golden.dCertGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.ScriptPurpose" $ take n Golden.scriptPurposeGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TxInfo" $ take n Golden.txInfoGoldensV1
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.ScriptContext" $ take n Golden.scriptContextGoldensV1
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.TxOut" $ take n Golden.txOutGoldensV1
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.TxInInfo" $ take n Golden.txInInfoGoldensV2
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.OutputDatum" $ take n Golden.outDatumGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.TxOut" $ take n Golden.txOutGoldensV2
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.TxInfo" $ take n Golden.txInfoGoldensV2
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV2.ScriptContext" $ take n Golden.scriptContextGoldensV2
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Rational" $ take n Golden.rationalGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.TxId" $ take n Golden.txIdGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.TxOutRef" $ take n Golden.txOutRefGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ColdCommitteeCredential" $ take n Golden.coldCommitteeCredentialGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.HotCommitteeCredential" $ take n Golden.hotCommitteeCredentialGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.DRepCredential" $ take n Golden.drepCredentialGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.DRep" $ take n Golden.drepGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Delegatee" $ take n Golden.delegateeGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV1.Lovelace" $ take n Golden.lovelaceGoldens
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.TxCert" $ take n Golden.txCertGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Voter" $ take n Golden.voterGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Vote" $ take n Golden.voteGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.GovernanceActionId" $ take n Golden.governanceActionIdGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Committee" $ take n Golden.committeeGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.Constitution" $ take n Golden.constitutionGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ProtocolVersion" $ take n Golden.protocolVersionGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ChangedParameters" $ take n Golden.changedParametersGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.GovernanceAction" $ take n Golden.governanceActionGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ProposalProcedure" $ take n Golden.proposalProcedureGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ScriptPurpose" $ take n Golden.scriptPurposeGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ScriptInfo" $ take n Golden.scriptInfoGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.TxInInfo" $ take n Golden.txInInfoGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.TxInfo" $ take n Golden.txInfoGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "PlutusV3.ScriptContext" $ take n Golden.scriptContextGoldensV3
      , GoldenPlutusData.writeGoldens goldenDir "Days.Day" $ take n Golden.dayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Days.WorkDay" $ take n Golden.workDayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Days.FreeDay" $ take n Golden.freeDayGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.A" $ take n Golden.aGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.B" $ take n Golden.bGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.C" $ take n Golden.cGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.D" $ take n Golden.dGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.FInt" $ take n Golden.fIntGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Foo.GInt" $ take n Golden.gIntGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Prelude.Bool" $ take n Golden.boolGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Prelude.Maybe" $ take n Golden.maybeGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Prelude.Either" $ take n Golden.eitherGoldens
      , GoldenPlutusData.writeGoldens goldenDir "Prelude.List" $ take n Golden.listGoldens
      ]
  putStrLn "[lbt-plutus-golden] Wrote PlutusData goldens:"
  for_ fps putStrLn
