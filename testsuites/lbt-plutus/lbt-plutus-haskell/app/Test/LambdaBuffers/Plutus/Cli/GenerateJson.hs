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
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Rational" $ take n Golden.rationalGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV3.TxId" $ take n Golden.txIdGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.TxOutRef" $ take n Golden.txOutRefGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ColdCommitteeCredential" $ take n Golden.coldCommitteeCredentialGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.HotCommitteeCredential" $ take n Golden.hotCommitteeCredentialGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.DRepCredential" $ take n Golden.drepCredentialGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.DRep" $ take n Golden.drepGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Delegatee" $ take n Golden.delegateeGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV1.Lovelace" $ take n Golden.lovelaceGoldens
      , GoldenJson.writeGoldens goldenDir "PlutusV3.TxCert" $ take n Golden.txCertGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Voter" $ take n Golden.voterGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Vote" $ take n Golden.voteGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.GovernanceActionId" $ take n Golden.governanceActionIdGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Committee" $ take n Golden.committeeGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.Constitution" $ take n Golden.constitutionGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ProtocolVersion" $ take n Golden.protocolVersionGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ChangedParameters" $ take n Golden.changedParametersGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.GovernanceAction" $ take n Golden.governanceActionGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ProposalProcedure" $ take n Golden.proposalProcedureGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ScriptPurpose" $ take n Golden.scriptPurposeGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ScriptInfo" $ take n Golden.scriptInfoGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.TxInInfo" $ take n Golden.txInInfoGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.TxInfo" $ take n Golden.txInfoGoldensV3
      , GoldenJson.writeGoldens goldenDir "PlutusV3.ScriptContext" $ take n Golden.scriptContextGoldensV3
      ]
  putStrLn "[lbt-plutus-golden] Wrote Json goldens:"
  for_ fps putStrLn
