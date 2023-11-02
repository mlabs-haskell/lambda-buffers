{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.LambdaBuffers.Runtime.Plutus.PlutusData (tests) where

import LambdaBuffers.Days qualified as HlDays
import LambdaBuffers.Days.Plutarch qualified as PlDays
import LambdaBuffers.Foo qualified as HlFoo
import LambdaBuffers.Foo.Plutarch qualified as PlFoo
import LambdaBuffers.Plutus.V1 qualified as HlPlutus
import LambdaBuffers.Plutus.V1.Plutarch qualified as PlPlutus
import LambdaBuffers.Plutus.V2 qualified as HlPlutusV2
import LambdaBuffers.Plutus.V2.Plutarch qualified as PlPlutusV2
import LambdaBuffers.Prelude qualified as HlPrelude
import LambdaBuffers.Prelude.Plutarch qualified as PlPrelude
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutarch.LamVal qualified as LbPl
import LambdaBuffers.Runtime.Plutus ()
import Plutarch (Config (Config), TracingMode (DoTracingAndBinds), pcon, perror, plam, pmatch, (#), (:-->))
import Plutarch qualified
import Plutarch.Bool (PBool, pif, (#==))
import Plutarch.Builtin (PData, pforgetData)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude (PAsData, PIsData, PTryFrom, pconstant)
import PlutusTx (Data, ToData)
import PlutusTx.IsData (FromData, toData)
import Test.LambdaBuffers.Plutus.Plutarch.Golden (readGoldenPdJson)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Round trip tests (from goldens and back)"
    [ forallGoldens @HlDays.Day @PlDays.Day "Days.Day" 6
    , forallGoldens @HlDays.FreeDay @PlDays.FreeDay "Days.FreeDay" 1
    , forallGoldens @HlDays.WorkDay @PlDays.WorkDay "Days.WorkDay" 4
    , forallGoldens @HlFoo.A @PlFoo.A "Foo.A" 9
    , forallGoldens @HlFoo.B @PlFoo.B "Foo.B" 9
    , forallGoldens @HlFoo.B @PlFoo.B "Foo.B" 9
    , forallGoldens @HlFoo.D @PlFoo.D "Foo.D" 7
    , forallGoldens @HlPlutus.Address @PlPlutus.Address "PlutusV1.Address" 7
    , forallGoldens @HlPlutus.AssetClass @PlPlutus.AssetClass "PlutusV1.AssetClass" 3
    , forallGoldens @HlPlutus.Bytes @PlPlutus.Bytes "PlutusV1.Bytes" 2
    , forallGoldens @HlPlutus.Credential @PlPlutus.Credential "PlutusV1.Credential" 1
    , forallGoldens @HlPlutus.CurrencySymbol @PlPlutus.CurrencySymbol "PlutusV1.CurrencySymbol" 1
    , forallGoldens @HlPlutus.Datum @PlPlutus.Datum "PlutusV1.Datum" 0
    , forallGoldens @HlPlutus.DatumHash @PlPlutus.DatumHash "PlutusV1.DatumHash" 0
    , forallGoldens @(HlPlutus.Extended HlPlutus.POSIXTime) @(PlPlutus.Extended PlPlutus.POSIXTime) "PlutusV1.Extended" 2
    , forallGoldens @(HlPlutus.Interval HlPlutus.POSIXTime) @(PlPlutus.Interval PlPlutus.POSIXTime) "PlutusV1.Interval" 9
    , forallGoldens @(HlPlutus.LowerBound HlPlutus.POSIXTime) @(PlPlutus.LowerBound PlPlutus.POSIXTime) "PlutusV1.LowerBound" 5
    , forallGoldens @(HlPlutus.Map HlPlutus.CurrencySymbol (HlPlutus.Map HlPlutus.TokenName HlPrelude.Integer)) @(PlPlutus.Map PlPlutus.CurrencySymbol (PlPlutus.Map PlPlutus.TokenName PlPrelude.Integer)) "PlutusV1.Map" 2
    , forallGoldens @HlPlutus.POSIXTime @PlPlutus.POSIXTime "PlutusV1.POSIXTime" 2
    , forallGoldens @HlPlutus.POSIXTimeRange @PlPlutus.POSIXTimeRange "PlutusV1.POSIXTimeRange" 9
    , forallGoldens @HlPlutus.PlutusData @PlPlutus.PlutusData "PlutusV1.PlutusData" 12
    , forallGoldens @HlPlutus.Redeemer @PlPlutus.Redeemer "PlutusV1.Redeemer" 0
    , forallGoldens @HlPlutus.RedeemerHash @PlPlutus.RedeemerHash "PlutusV1.RedeemerHash" 0
    , forallGoldens @HlPlutus.ScriptHash @PlPlutus.ScriptHash "PlutusV1.ScriptHash" 0
    , forallGoldens @HlPlutus.StakingCredential @PlPlutus.StakingCredential "PlutusV1.StakingCredential" 2
    , forallGoldens @HlPlutus.TokenName @PlPlutus.TokenName "PlutusV1.TokenName" 2
    , forallGoldens @HlPlutus.TxId @PlPlutus.TxId "PlutusV1.TxId" 0
    , forallGoldens @HlPlutus.TxOutRef @PlPlutus.TxOutRef "PlutusV1.TxOutRef" 0
    , forallGoldens @(HlPlutus.UpperBound HlPlutus.POSIXTime) @(PlPlutus.UpperBound PlPlutus.POSIXTime) "PlutusV1.UpperBound" 5
    , forallGoldens @HlPlutus.Value @PlPlutus.Value "PlutusV1.Value" 2
    , forallGoldens @HlPlutusV2.OutputDatum @PlPlutusV2.OutputDatum "PlutusV2.OutputDatum" 2
    , forallGoldens @HlPlutusV2.TxInInfo @PlPlutusV2.TxInInfo "PlutusV2.TxInInfo" 9
    , forallGoldens @HlPlutusV2.TxOut @PlPlutusV2.TxOut "PlutusV2.TxOut" 9
    ]

evalRoundTrip :: forall a. (PIsData a, PTryFrom PData (PAsData a)) => Data -> Assertion
evalRoundTrip pd = case Plutarch.compile (Config DoTracingAndBinds) (roundTripFunction @a # pconstant pd) of
  Left err -> assertFailure $ show ("Error while evaluating a Plutarch Term", err)
  Right script -> case evalScript script of
    (Left err, _, trace) -> assertFailure $ show ("Error while evaluating a Plutarch Term", err, trace)
    _ -> return ()

roundTripFunction :: forall a s. (PIsData a, PTryFrom PData (PAsData a)) => Plutarch.Term s (PData :--> PBool)
roundTripFunction =
  plam $ \pd ->
    pmatch
      (LbPl.pfromPlutusDataPTryFrom @a # pd)
      ( \x ->
          pif
            ((pforgetData . pcon $ x) #== pd)
            (pconstant True)
            perror
      )

roundTripTestCase :: forall a a'. (ToData a, FromData a, PIsData a', PTryFrom PData (PAsData a')) => FilePath -> TestTree
roundTripTestCase fp = testCase fp $ do
  x <- readGoldenPdJson @a fp
  evalRoundTrip @a' (toData @a x)

forallGoldens :: forall a a'. (ToData a, FromData a, PIsData a', PTryFrom PData (PAsData a')) => FilePath -> Int -> TestTree
forallGoldens prefix howMany = testGroup prefix $ fmap (\i -> roundTripTestCase @a @a' (prefix <> "." <> show i <> ".pd.json")) [0 .. howMany]

_goldens :: [String]
_goldens =
  [ "Days.Day.0.pd.json"
  , "Days.Day.1.pd.json"
  , "Days.Day.2.pd.json"
  , "Days.Day.3.pd.json"
  , "Days.Day.4.pd.json"
  , "Days.Day.5.pd.json"
  , "Days.Day.6.pd.json"
  , "Days.FreeDay.0.pd.json"
  , "Days.FreeDay.1.pd.json"
  , "Days.WorkDay.0.pd.json"
  , "Days.WorkDay.1.pd.json"
  , "Days.WorkDay.2.pd.json"
  , "Days.WorkDay.3.pd.json"
  , "Days.WorkDay.4.pd.json"
  , "Foo.A.0.pd.json"
  , "Foo.A.1.pd.json"
  , "Foo.A.2.pd.json"
  , "Foo.A.3.pd.json"
  , "Foo.A.4.pd.json"
  , "Foo.A.5.pd.json"
  , "Foo.A.6.pd.json"
  , "Foo.A.7.pd.json"
  , "Foo.A.8.pd.json"
  , "Foo.A.9.pd.json"
  , "Foo.B.0.pd.json"
  , "Foo.B.1.pd.json"
  , "Foo.B.2.pd.json"
  , "Foo.B.3.pd.json"
  , "Foo.B.4.pd.json"
  , "Foo.B.5.pd.json"
  , "Foo.B.6.pd.json"
  , "Foo.B.7.pd.json"
  , "Foo.B.8.pd.json"
  , "Foo.B.9.pd.json"
  , "Foo.C.0.pd.json"
  , "Foo.C.1.pd.json"
  , "Foo.C.2.pd.json"
  , "Foo.C.3.pd.json"
  , "Foo.C.4.pd.json"
  , "Foo.C.5.pd.json"
  , "Foo.C.6.pd.json"
  , "Foo.C.7.pd.json"
  , "Foo.C.8.pd.json"
  , "Foo.C.9.pd.json"
  , "Foo.D.0.pd.json"
  , "Foo.D.1.pd.json"
  , "Foo.D.2.pd.json"
  , "Foo.D.3.pd.json"
  , "Foo.D.4.pd.json"
  , "Foo.D.5.pd.json"
  , "Foo.D.6.pd.json"
  , "Foo.D.7.pd.json"
  , "PlutusV1.Address.0.pd.json"
  , "PlutusV1.Address.1.pd.json"
  , "PlutusV1.Address.2.pd.json"
  , "PlutusV1.Address.3.pd.json"
  , "PlutusV1.Address.4.pd.json"
  , "PlutusV1.Address.5.pd.json"
  , "PlutusV1.Address.6.pd.json"
  , "PlutusV1.Address.7.pd.json"
  , "PlutusV1.AssetClass.0.pd.json"
  , "PlutusV1.AssetClass.1.pd.json"
  , "PlutusV1.AssetClass.2.pd.json"
  , "PlutusV1.AssetClass.3.pd.json"
  , "PlutusV1.Bytes.0.pd.json"
  , "PlutusV1.Bytes.1.pd.json"
  , "PlutusV1.Bytes.2.pd.json"
  , "PlutusV1.Credential.0.pd.json"
  , "PlutusV1.Credential.1.pd.json"
  , "PlutusV1.CurrencySymbol.0.pd.json"
  , "PlutusV1.CurrencySymbol.1.pd.json"
  , "PlutusV1.Datum.0.pd.json"
  , "PlutusV1.DatumHash.0.pd.json"
  , "PlutusV1.Extended.0.pd.json"
  , "PlutusV1.Extended.1.pd.json"
  , "PlutusV1.Extended.2.pd.json"
  , "PlutusV1.Interval.0.pd.json"
  , "PlutusV1.Interval.1.pd.json"
  , "PlutusV1.Interval.2.pd.json"
  , "PlutusV1.Interval.3.pd.json"
  , "PlutusV1.Interval.4.pd.json"
  , "PlutusV1.Interval.5.pd.json"
  , "PlutusV1.Interval.6.pd.json"
  , "PlutusV1.Interval.7.pd.json"
  , "PlutusV1.Interval.8.pd.json"
  , "PlutusV1.Interval.9.pd.json"
  , "PlutusV1.LowerBound.0.pd.json"
  , "PlutusV1.LowerBound.1.pd.json"
  , "PlutusV1.LowerBound.2.pd.json"
  , "PlutusV1.LowerBound.3.pd.json"
  , "PlutusV1.LowerBound.4.pd.json"
  , "PlutusV1.LowerBound.5.pd.json"
  , "PlutusV1.Map.0.pd.json"
  , "PlutusV1.Map.1.pd.json"
  , "PlutusV1.Map.2.pd.json"
  , "PlutusV1.POSIXTime.0.pd.json"
  , "PlutusV1.POSIXTime.1.pd.json"
  , "PlutusV1.POSIXTime.2.pd.json"
  , "PlutusV1.POSIXTimeRange.0.pd.json"
  , "PlutusV1.POSIXTimeRange.1.pd.json"
  , "PlutusV1.POSIXTimeRange.2.pd.json"
  , "PlutusV1.POSIXTimeRange.3.pd.json"
  , "PlutusV1.POSIXTimeRange.4.pd.json"
  , "PlutusV1.POSIXTimeRange.5.pd.json"
  , "PlutusV1.POSIXTimeRange.6.pd.json"
  , "PlutusV1.POSIXTimeRange.7.pd.json"
  , "PlutusV1.POSIXTimeRange.8.pd.json"
  , "PlutusV1.POSIXTimeRange.9.pd.json"
  , "PlutusV1.PlutusData.0.pd.json"
  , "PlutusV1.PlutusData.1.pd.json"
  , "PlutusV1.PlutusData.10.pd.json"
  , "PlutusV1.PlutusData.11.pd.json"
  , "PlutusV1.PlutusData.12.pd.json"
  , "PlutusV1.PlutusData.2.pd.json"
  , "PlutusV1.PlutusData.3.pd.json"
  , "PlutusV1.PlutusData.4.pd.json"
  , "PlutusV1.PlutusData.5.pd.json"
  , "PlutusV1.PlutusData.6.pd.json"
  , "PlutusV1.PlutusData.7.pd.json"
  , "PlutusV1.PlutusData.8.pd.json"
  , "PlutusV1.PlutusData.9.pd.json"
  , "PlutusV1.PubKeyHash.0.pd.json"
  , "PlutusV1.Redeemer.0.pd.json"
  , "PlutusV1.RedeemerHash.0.pd.json"
  , "PlutusV1.ScriptHash.0.pd.json"
  , "PlutusV1.StakingCredential.0.pd.json"
  , "PlutusV1.StakingCredential.1.pd.json"
  , "PlutusV1.StakingCredential.2.pd.json"
  , "PlutusV1.TokenName.0.pd.json"
  , "PlutusV1.TokenName.1.pd.json"
  , "PlutusV1.TokenName.2.pd.json"
  , "PlutusV1.TxId.0.pd.json"
  , "PlutusV1.TxOutRef.0.pd.json"
  , "PlutusV1.UpperBound.0.pd.json"
  , "PlutusV1.UpperBound.1.pd.json"
  , "PlutusV1.UpperBound.2.pd.json"
  , "PlutusV1.UpperBound.3.pd.json"
  , "PlutusV1.UpperBound.4.pd.json"
  , "PlutusV1.UpperBound.5.pd.json"
  , "PlutusV1.Value.0.pd.json"
  , "PlutusV1.Value.1.pd.json"
  , "PlutusV1.Value.2.pd.json"
  , "PlutusV2.OutputDatum.0.pd.json"
  , "PlutusV2.OutputDatum.1.pd.json"
  , "PlutusV2.OutputDatum.2.pd.json"
  , "PlutusV2.TxInInfo.0.pd.json"
  , "PlutusV2.TxInInfo.1.pd.json"
  , "PlutusV2.TxInInfo.2.pd.json"
  , "PlutusV2.TxInInfo.3.pd.json"
  , "PlutusV2.TxInInfo.4.pd.json"
  , "PlutusV2.TxInInfo.5.pd.json"
  , "PlutusV2.TxInInfo.6.pd.json"
  , "PlutusV2.TxInInfo.7.pd.json"
  , "PlutusV2.TxInInfo.8.pd.json"
  , "PlutusV2.TxInInfo.9.pd.json"
  , "PlutusV2.TxOut.0.pd.json"
  , "PlutusV2.TxOut.1.pd.json"
  , "PlutusV2.TxOut.2.pd.json"
  , "PlutusV2.TxOut.3.pd.json"
  , "PlutusV2.TxOut.4.pd.json"
  , "PlutusV2.TxOut.5.pd.json"
  , "PlutusV2.TxOut.6.pd.json"
  , "PlutusV2.TxOut.7.pd.json"
  , "PlutusV2.TxOut.8.pd.json"
  , "PlutusV2.TxOut.9.pd.json"
  ]
