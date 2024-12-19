{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Test.LambdaBuffers.Runtime.PlutusTx.PlutusData (tests) where

import Data.ByteString qualified as B
import LambdaBuffers.Days qualified as HlDays
import LambdaBuffers.Days.PlutusTx qualified as PlDays
import LambdaBuffers.Foo qualified as HlFoo
import LambdaBuffers.Foo.PlutusTx qualified as PlFoo
import LambdaBuffers.Plutus.V1 qualified as HlPlutus
import LambdaBuffers.Plutus.V1.PlutusTx qualified as PlPlutus
import LambdaBuffers.Plutus.V2 qualified as HlPlutusV2
import LambdaBuffers.Plutus.V2.PlutusTx qualified as PlPlutusV2
import LambdaBuffers.Plutus.V3 qualified as HlPlutusV3
import LambdaBuffers.Plutus.V3.PlutusTx qualified as PlPlutusV3
import LambdaBuffers.Prelude qualified as HlPrelude
import LambdaBuffers.Prelude.PlutusTx qualified as PlPrelude
import LambdaBuffers.Runtime.Plutus ()
import LambdaBuffers.Runtime.PlutusTx ()
import LambdaBuffers.Runtime.Prelude.Json qualified as Lb
import Paths_lbt_plutus_golden_data qualified as Paths
import PlutusTx (BuiltinData, CompiledCode, ToData, dataToBuiltinData)
import PlutusTx.IsData (FromData, fromData, toData)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Test.LambdaBuffers.Runtime.PlutusTx.Evaluate qualified as Evaluate
import Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Prelude

-- TODO(bladyjoker): Make the ``xyzCompiled` functions have `BuiltinData -> a` type so the `plutusType` type argument is actually used
tests :: TestTree
tests =
  testGroup
    "Round trip tests (from goldens and back)"
    [ transparentGoldens
    , preludeGoldens
    , plutusV1Goldens
    , plutusV2Goldens
    , plutusV3Goldens
    ]

transparentGoldens :: TestTree
transparentGoldens =
  testGroup
    "Transparent golden types"
    [ forallGoldens @HlDays.Day @PlDays.Day PlutusTx.dayCompiled "Days.Day" 6
    , forallGoldens @HlDays.FreeDay @PlDays.FreeDay PlutusTx.freeDayCompiled "Days.FreeDay" 1
    , forallGoldens @HlDays.WorkDay @PlDays.WorkDay PlutusTx.workDayCompiled "Days.WorkDay" 4
    , forallGoldens @HlFoo.A @PlFoo.A PlutusTx.fooACompiled "Foo.A" 9
    , forallGoldens @HlFoo.B @PlFoo.B PlutusTx.fooBCompiled "Foo.B" 9
    , forallGoldens @HlFoo.C @PlFoo.C PlutusTx.fooCCompiled "Foo.C" 9
    , forallGoldens @HlFoo.D @PlFoo.D PlutusTx.fooDCompiled "Foo.D" 7
    , -- PlutusTX compilation caused OOM
      -- , ignoreTestBecause "TODO(bladyjoker): What happened to Foo.E goldens? Install them!" $ forallGoldens @(HlFoo.E Integer Bool) @(PlFoo.E PlutusTx.Integer PlutusTx.Bool) PlutusTx.fooECompiled "Foo.E" 1
      ignoreTestBecause "GHC Core to PLC plugin: E003:Error: Error from the PIR compiler: E003: Unsupported construct: Mutually recursive datatypes ((recursive) let binding; from [ AnnOther ])" $ testCase "Foo.FInt" (print ("Not compiling" :: String))
    , ignoreTestBecause "GHC Core to PLC plugin: E003:Error: Error from the PIR compiler: E003: Unsupported construct: Mutually recursive datatypes ((recursive) let binding; from [ AnnOther ])" $ testCase "Foo.GInt" (print ("Not compiling" :: String))
    ]

preludeGoldens :: TestTree
preludeGoldens =
  testGroup
    "LB Prelude golden types"
    [ forallGoldens @(HlPrelude.Maybe HlPrelude.Bool) @(PlPrelude.Maybe PlPrelude.Bool) PlutusTx.maybeCompiled "Prelude.Maybe" 2
    , forallGoldens @(HlPrelude.Either HlPrelude.Bool HlPrelude.Bool) @(PlPrelude.Either PlPrelude.Bool PlPrelude.Bool) PlutusTx.eitherCompiled "Prelude.Either" 2
    , forallGoldens @(HlPrelude.List HlPrelude.Bool) @(PlPrelude.List PlPrelude.Bool) PlutusTx.listCompiled "Prelude.List" 3
    , ignoreTestBecause "TODO(bladyjoker): Include Prelude.Integer in the goldens" $ forallGoldens @HlPrelude.Integer @PlPrelude.Integer PlutusTx.integerCompiled "Prelude.Integer" 1
    ]

plutusV1Goldens :: TestTree
plutusV1Goldens =
  testGroup
    "LB Plutus.V1. golden types"
    [ forallGoldens @HlPlutus.Address @PlPlutus.Address PlutusTx.addressCompiled "PlutusV1.Address" 7
    , forallGoldens @HlPlutus.AssetClass @PlPlutus.AssetClass PlutusTx.assetClassCompiled "PlutusV1.AssetClass" 3
    , forallGoldens @HlPlutus.Bytes @PlPlutus.Bytes PlutusTx.ledgerBytesCompiled "PlutusV1.Bytes" 2
    , forallGoldens @HlPlutus.Credential @PlPlutus.Credential PlutusTx.credentialCompiled "PlutusV1.Credential" 1
    , forallGoldens @HlPlutus.CurrencySymbol @PlPlutus.CurrencySymbol PlutusTx.currencySymbolCompiled "PlutusV1.CurrencySymbol" 1
    , forallGoldens @HlPlutus.Datum @PlPlutus.Datum PlutusTx.datumCompiled "PlutusV1.Datum" 0
    , forallGoldens @HlPlutus.DatumHash @PlPlutus.DatumHash PlutusTx.datumHashCompiled "PlutusV1.DatumHash" 0
    , forallGoldens @(HlPlutus.Extended HlPlutus.POSIXTime) @(PlPlutus.Extended PlPlutus.POSIXTime) PlutusTx.extendedCompiled "PlutusV1.Extended" 2
    , forallGoldens @(HlPlutus.Interval HlPlutus.POSIXTime) @(PlPlutus.Interval PlPlutus.POSIXTime) PlutusTx.intervalCompiled "PlutusV1.Interval" 9
    , forallGoldens @(HlPlutus.LowerBound HlPlutus.POSIXTime) @(PlPlutus.LowerBound PlPlutus.POSIXTime) PlutusTx.lowerBoundCompiled "PlutusV1.LowerBound" 5
    , -- , forallGoldens @(HlPlutus.Map HlPlutus.CurrencySymbol (HlPlutus.Map HlPlutus.TokenName HlPrelude.Integer)) @(PlPlutus.Map PlPlutus.CurrencySymbol (PlPlutus.Map PlPlutus.TokenName PlPrelude.Integer)) PlutusTx.mapCompiled "PlutusV1.Map" 2
      forallGoldens @HlPlutus.POSIXTime @PlPlutus.POSIXTime PlutusTx.posixTimeCompiled "PlutusV1.POSIXTime" 2
    , forallGoldens @HlPlutus.POSIXTimeRange @PlPlutus.POSIXTimeRange PlutusTx.posixTimeRangeCompiled "PlutusV1.POSIXTimeRange" 9
    , forallGoldens @HlPlutus.PlutusData @PlPlutus.PlutusData PlutusTx.plutusDataCompiled "PlutusV1.PlutusData" 12
    , forallGoldens @HlPlutus.Redeemer @PlPlutus.Redeemer PlutusTx.redeemerCompiled "PlutusV1.Redeemer" 0
    , forallGoldens @HlPlutus.RedeemerHash @PlPlutus.RedeemerHash PlutusTx.redeemerHashCompiled "PlutusV1.RedeemerHash" 0
    , forallGoldens @HlPlutus.ScriptHash @PlPlutus.ScriptHash PlutusTx.scriptHashCompiled "PlutusV1.ScriptHash" 0
    , forallGoldens @HlPlutus.StakingCredential @PlPlutus.StakingCredential PlutusTx.stakingCredentialCompiled "PlutusV1.StakingCredential" 2
    , forallGoldens @HlPlutus.TokenName @PlPlutus.TokenName PlutusTx.tokenNameCompiled "PlutusV1.TokenName" 2
    , forallGoldens @HlPlutus.TxId @PlPlutus.TxId PlutusTx.txIdCompiled "PlutusV1.TxId" 0
    , forallGoldens @HlPlutus.TxOutRef @PlPlutus.TxOutRef PlutusTx.txOutRefCompiled "PlutusV1.TxOutRef" 0
    , forallGoldens @(HlPlutus.UpperBound HlPlutus.POSIXTime) @(PlPlutus.UpperBound PlPlutus.POSIXTime) PlutusTx.upperBoundCompiled "PlutusV1.UpperBound" 5
    , forallGoldens @HlPlutus.Value @PlPlutus.Value PlutusTx.valueCompiled "PlutusV1.Value" 2
    , forallGoldens @HlPlutus.DCert @PlPlutus.DCert PlutusTx.dcertCompiled "PlutusV1.DCert" 9
    , forallGoldens @HlPlutus.TxOut @PlPlutus.TxOut PlutusTx.txOutCompiled "PlutusV1.TxOut" 9
    , forallGoldens @HlPlutus.TxInInfo @PlPlutus.TxInInfo PlutusTx.txInInfoCompiled "PlutusV1.TxInInfo" 9
    , forallGoldens @HlPlutus.ScriptPurpose @PlPlutus.ScriptPurpose PlutusTx.scriptPurposeCompiled "PlutusV1.ScriptPurpose" 9
    , forallGoldens @HlPlutus.TxInfo @PlPlutus.TxInfo PlutusTx.txInfoCompiled "PlutusV1.TxInfo" 9
    , forallGoldens @HlPlutus.ScriptContext @PlPlutus.ScriptContext PlutusTx.scriptContextCompiled "PlutusV1.ScriptContext" 9
    ]

plutusV2Goldens :: TestTree
plutusV2Goldens =
  testGroup
    "LB Plutus.V2 golden types"
    [ forallGoldens @HlPlutusV2.OutputDatum @PlPlutusV2.OutputDatum PlutusTx.outputDatumCompiled "PlutusV2.OutputDatum" 2
    , forallGoldens @HlPlutusV2.TxInInfo @PlPlutusV2.TxInInfo PlutusTx.txInInfo2Compiled "PlutusV2.TxInInfo" 9
    , forallGoldens @HlPlutusV2.TxOut @PlPlutusV2.TxOut PlutusTx.txOut2Compiled "PlutusV2.TxOut" 9
    -- , forallGoldens @HlPlutusV2.TxInfo @PlPlutusV2.TxInfo PlutusTx.txInfo2Compiled "PlutusV2.TxInfo" 9
    -- , forallGoldens @HlPlutusV2.ScriptContext @PlPlutusV2.ScriptContext PlutusTx.scriptContext2Compiled "PlutusV2.ScriptContext" 9
    ]

plutusV3Goldens :: TestTree
plutusV3Goldens =
  testGroup
    "LB Plutus.V3 golden types"
    [ forallGoldens @HlPlutusV3.Rational @PlPlutusV3.Rational PlutusTx.rationalCompiled "PlutusV3.Rational" 0
    , forallGoldens @HlPlutusV3.TxId @PlPlutusV3.TxId PlutusTx.txIdV3Compiled "PlutusV3.TxId" 0
    , forallGoldens @HlPlutusV3.TxOutRef @PlPlutusV3.TxOutRef PlutusTx.txOutRefV3Compiled "PlutusV3.TxOutRef" 0
    , forallGoldens @HlPlutusV3.ColdCommitteeCredential @PlPlutusV3.ColdCommitteeCredential PlutusTx.coldCommitteeCredentialV3Compiled "PlutusV3.ColdCommitteeCredential" 1
    , forallGoldens @HlPlutusV3.HotCommitteeCredential @PlPlutusV3.HotCommitteeCredential PlutusTx.hotCommitteeCredentialV3Compiled "PlutusV3.HotCommitteeCredential" 1
    , forallGoldens @HlPlutusV3.DRepCredential @PlPlutusV3.DRepCredential PlutusTx.drepCredentialV3Compiled "PlutusV3.DRepCredential" 1
    , forallGoldens @HlPlutusV3.DRep @PlPlutusV3.DRep PlutusTx.drepV3Compiled "PlutusV3.DRep" 3
    , forallGoldens @HlPlutusV3.Delegatee @PlPlutusV3.Delegatee PlutusTx.delegateeV3Compiled "PlutusV3.Delegatee" 8
    , forallGoldens @HlPlutusV3.TxCert @PlPlutusV3.TxCert PlutusTx.txCertV3Compiled "PlutusV3.TxCert" 9
    , forallGoldens @HlPlutusV3.Voter @PlPlutusV3.Voter PlutusTx.voterV3Compiled "PlutusV3.Voter" 4
    , forallGoldens @HlPlutusV3.Vote @PlPlutusV3.Vote PlutusTx.voteV3Compiled "PlutusV3.Vote" 2
    , forallGoldens @HlPlutusV3.GovernanceActionId @PlPlutusV3.GovernanceActionId PlutusTx.governanceActionIdV3Compiled "PlutusV3.GovernanceActionId" 0
    , -- , forallGoldens @HlPlutusV3.Committee @PlPlutusV3.Committee PlutusTx.committeeV3Compiled "PlutusV3.Committee"  9
      forallGoldens @HlPlutusV3.Constitution @PlPlutusV3.Constitution PlutusTx.constitutionV3Compiled "PlutusV3.Constitution" 1
    , forallGoldens @HlPlutusV3.ProtocolVersion @PlPlutusV3.ProtocolVersion PlutusTx.protocolVersionV3Compiled "PlutusV3.ProtocolVersion" 0
    , forallGoldens @HlPlutusV3.ChangedParameters @PlPlutusV3.ChangedParameters PlutusTx.changedParametersV3Compiled "PlutusV3.ChangedParameters" 9
    -- , forallGoldens @HlPlutusV3.GovernanceAction @PlPlutusV3.GovernanceAction PlutusTx.governanceActionV3Compiled "PlutusV3.GovernanceAction"  9
    -- , forallGoldens @HlPlutusV3.ProposalProcedure @PlPlutusV3.ProposalProcedure PlutusTx.proposalProcedureV3Compiled "PlutusV3.ProposalProcedure"  9
    -- , forallGoldens @HlPlutusV3.ScriptPurpose @PlPlutusV3.ScriptPurpose PlutusTx.scriptPurposeV3Compiled "PlutusV3.ScriptPurpose"  9
    -- , forallGoldens @HlPlutusV3.ScriptInfo @PlPlutusV3.ScriptInfo PlutusTx.scriptInfoV3Compiled "PlutusV3.ScriptInfo"  9
    -- , forallGoldens @HlPlutusV3.TxInInfo @PlPlutusV3.TxInInfo PlutusTx.txInInfoV3Compiled "PlutusV3.TxInInfo"  9
    -- , forallGoldens @HlPlutusV3.TxInfo @PlPlutusV3.TxInfo PlutusTx.txInfoV3Compiled "PlutusV3.TxInfo"  9
    -- , forallGoldens @HlPlutusV3.ScriptContext @PlPlutusV3.ScriptContext PlutusTx.scriptContextV3Compiled "PlutusV3.ScriptContext"  9
    ]

forallGoldens ::
  forall haskellType plutusTxType.
  ( FromData haskellType
  , ToData haskellType
  ) =>
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool) ->
  FilePath ->
  Int ->
  TestTree
forallGoldens plutusTxFunction prefix howMany =
  testGroup prefix $
    fmap
      ( \i ->
          roundTripTestCase @haskellType
            plutusTxFunction
            (prefix <> "." <> show i <> ".pd.json")
      )
      [0 .. howMany]

roundTripTestCase ::
  forall haskellType.
  ( FromData haskellType
  , ToData haskellType
  ) =>
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool) ->
  FilePath ->
  TestTree
roundTripTestCase plutusTxFunction fp = testCase fp $ do
  x <- readGoldenPdJson @haskellType fp
  evalRoundTrip plutusTxFunction (dataToBuiltinData $ toData @haskellType x)

evalRoundTrip :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool) -> BuiltinData -> Assertion
evalRoundTrip plutusTxFunction pd = case Evaluate.applyArg plutusTxFunction pd of
  Left err -> assertFailure $ show ("Error while applying a PlutusData value to a PlutusTx Term" :: String, err)
  Right cc -> case Evaluate.evalScriptHuge . Evaluate.fromCompiledCode $ cc of
    (Left err, _, trace) -> assertFailure $ show ("Error while evaluating a PlutusTx script" :: String, err, trace)
    _ -> return ()

-- | Golden utilities
readPdJson :: FromData b => FilePath -> IO b
readPdJson fp = do
  content <- B.readFile fp
  case Lb.fromJsonBytes content of
    Left err -> do
      print ("Error while parsing LambdaBuffers .pd.json file" :: String, fp, err)
      exitFailure
    Right pd -> do
      case fromData pd of
        Nothing -> do
          print ("Error while parsing LambdaBuffers PlutusData" :: String, fp)
          exitFailure
        Just x -> return x

readGoldenPdJson :: FromData b => FilePath -> IO b
readGoldenPdJson fp = do
  dataDir <- Paths.getDataDir
  readPdJson (dataDir </> "data" </> fp)
