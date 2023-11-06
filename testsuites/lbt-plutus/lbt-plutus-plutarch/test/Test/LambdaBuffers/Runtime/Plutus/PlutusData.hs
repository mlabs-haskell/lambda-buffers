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
import Plutarch.Builtin (PBuiltinList, PData, pforgetData)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude (PAsData, PIsData, PTryFrom, pconstant, pdata)
import PlutusTx (Data, ToData)
import PlutusTx.IsData (FromData, toData)
import Test.LambdaBuffers.Plutus.Plutarch.Golden (readGoldenPdJson)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
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
    , ignoreTestBecause "TODO(#131): Plutarch codegen: Recursive data type support" $ forallGoldens @HlFoo.FInt @PlFoo.FInt "Foo.FInt" 1
    , ignoreTestBecause "TODO(#131): Plutarch codegen: Recursive data type support" $ forallGoldens @HlFoo.GInt @PlFoo.GInt "Foo.GInt" 1
    , forallGoldens @(HlPrelude.Maybe HlPrelude.Bool) @(PlPrelude.Maybe PlPrelude.Bool) "Prelude.Maybe" 2
    , forallGoldens @(HlPrelude.Either HlPrelude.Bool HlPrelude.Bool) @(PlPrelude.Either PlPrelude.Bool PlPrelude.Bool) "Prelude.Either" 2
    , ignoreTestBecause "TODO(bladyjoker): PList test fails because `#==` triggers the PData instance PEq and not `PBuiltinList` which is its inner." $ forallGoldens @(HlPrelude.List HlPrelude.Bool) @(PlPrelude.List PlPrelude.Bool) "Prelude.List" 3
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
            (LbPl.toPlutusData (pcon x) #== pd)
            (pconstant True)
            perror
      )

roundTripTestCase :: forall a a'. (ToData a, FromData a, PIsData a', PTryFrom PData (PAsData a')) => FilePath -> TestTree
roundTripTestCase fp = testCase fp $ do
  x <- readGoldenPdJson @a fp
  evalRoundTrip @a' (toData @a x)

forallGoldens :: forall a a'. (ToData a, FromData a, PIsData a', PTryFrom PData (PAsData a')) => FilePath -> Int -> TestTree
forallGoldens prefix howMany = testGroup prefix $ fmap (\i -> roundTripTestCase @a @a' (prefix <> "." <> show i <> ".pd.json")) [0 .. howMany]
