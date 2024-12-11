{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx (
  dayCompiled,
  integerCompiled,
  boolCompiled,
  workDayCompiled,
  freeDayCompiled,
  fooACompiled,
  fooBCompiled,
  fooCCompiled,
  fooDCompiled,
  -- fooECompiled, -- Compiling this causes an OOM
  maybeCompiled,
  eitherCompiled,
  listCompiled,
  addressCompiled,
  assetClassCompiled,
  currencySymbolCompiled,
  credentialCompiled,
  -- mapCompiled, -- AssocMap has no PlutusTx.Eq instance
  intervalCompiled,
  extendedCompiled,
  lowerBoundCompiled,
  datumCompiled,
  datumHashCompiled,
  ledgerBytesCompiled,
  posixTimeCompiled,
  posixTimeRangeCompiled,
  plutusDataCompiled,
  redeemerCompiled,
  redeemerHashCompiled,
  scriptHashCompiled,
  stakingCredentialCompiled,
  tokenNameCompiled,
  txIdCompiled,
  txOutRefCompiled,
  upperBoundCompiled,
  valueCompiled,
  dcertCompiled,
  txInInfoCompiled,
  txOutCompiled,
  scriptPurposeCompiled,
  txInfoCompiled,
  scriptContextCompiled,
  outputDatumCompiled,
  txInInfo2Compiled,
  txOut2Compiled,
  -- txInfo2Compiled,
  -- scriptContext2Compiled,
) where

import LambdaBuffers.Days.PlutusTx (Day, FreeDay, WorkDay)
import LambdaBuffers.Foo.PlutusTx (A, B, C, D, E, FInt, GInt)
import LambdaBuffers.Plutus.V2.PlutusTx qualified as PlutusV2
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusTx (BuiltinData, CompiledCode, FromData (fromBuiltinData), ToData (toBuiltinData), compile)

-- import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Maybe (Maybe (Just, Nothing))
import PlutusTx.Plugin ()
import PlutusTx.Prelude (Bool, Either, Eq ((==)), Integer, error, trace, (&&))

{-# INLINEABLE fromToDataAndEq #-}
fromToDataAndEq :: forall a. (PlutusTx.Prelude.Eq a, FromData a, ToData a) => BuiltinData -> Bool
fromToDataAndEq x'data =
  let
    may'x = fromBuiltinData @a x'data
   in
    case may'x of
      Nothing -> trace "Failed FromData" (error ())
      Just x -> x == x && toBuiltinData x == x'data

integerCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
integerCompiled = $$(PlutusTx.compile [||fromToDataAndEq @Integer||])

boolCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
boolCompiled = $$(PlutusTx.compile [||fromToDataAndEq @Bool||])

dayCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
dayCompiled = $$(PlutusTx.compile [||fromToDataAndEq @Day||])

freeDayCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
freeDayCompiled = $$(PlutusTx.compile [||fromToDataAndEq @FreeDay||])

workDayCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
workDayCompiled = $$(PlutusTx.compile [||fromToDataAndEq @WorkDay||])

fooACompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooACompiled = $$(PlutusTx.compile [||fromToDataAndEq @A||])

fooBCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooBCompiled = $$(PlutusTx.compile [||fromToDataAndEq @B||])

fooCCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooCCompiled = $$(PlutusTx.compile [||fromToDataAndEq @C||])

fooDCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooDCompiled = $$(PlutusTx.compile [||fromToDataAndEq @D||])

fooECompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooECompiled = $$(PlutusTx.compile [||fromToDataAndEq @(E Integer Bool)||])

-- NOTE(bladyjoker): Recursive types are not supported by PlutusTx
fooFIntCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooFIntCompiled = $$(PlutusTx.compile [||fromToDataAndEq @FInt||])

-- NOTE(bladyjoker): Recursive types are not supported by PlutusTx
fooGIntCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooGIntCompiled = $$(PlutusTx.compile [||fromToDataAndEq @GInt||])

-- * Prelude

maybeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
maybeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(Maybe Bool)||])

eitherCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
eitherCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(Either Bool Bool)||])

listCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
listCompiled = $$(PlutusTx.compile [||fromToDataAndEq @[Bool]||])

-- * Plutus V1
addressCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
addressCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.Address||])

assetClassCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
assetClassCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.AssetClass||])

ledgerBytesCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
ledgerBytesCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.LedgerBytes||])

credentialCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
credentialCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.Credential||])

currencySymbolCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
currencySymbolCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.CurrencySymbol||])

datumCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
datumCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.Datum||])

datumHashCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
datumHashCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.DatumHash||])

extendedCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
extendedCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(PlutusV1.Extended PlutusV1.POSIXTime)||])

intervalCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
intervalCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(PlutusV1.Interval PlutusV1.POSIXTime)||])

lowerBoundCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
lowerBoundCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(PlutusV1.LowerBound PlutusV1.POSIXTime)||])

-- mapCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
-- mapCompiled = $$(PlutusTx.compile [||fromToData @(PlutusTx.Map PlutusV1.CurrencySymbol (PlutusTx.Map PlutusV1.TokenName Integer))||])

posixTimeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
posixTimeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.POSIXTime||])

posixTimeRangeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
posixTimeRangeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.POSIXTimeRange||])

plutusDataCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
plutusDataCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.BuiltinData||])

redeemerCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
redeemerCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.Redeemer||])

redeemerHashCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
redeemerHashCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.RedeemerHash||])

scriptHashCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptHashCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.ScriptHash||])

stakingCredentialCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
stakingCredentialCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.StakingCredential||])

tokenNameCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
tokenNameCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TokenName||])

txIdCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txIdCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TxId||])

txOutRefCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txOutRefCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TxOutRef||])

upperBoundCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
upperBoundCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(PlutusV1.UpperBound PlutusV1.POSIXTime)||])

valueCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
valueCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.Value||])

dcertCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
dcertCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.DCert||])

scriptPurposeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptPurposeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.ScriptPurpose||])

txInInfoCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txInInfoCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TxInInfo||])

txOutCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txOutCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TxOut||])

txInfoCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txInfoCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.TxInfo||])

scriptContextCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptContextCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.ScriptContext||])

-- * Plutus V2
outputDatumCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
outputDatumCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV2.OutputDatum||])

txInInfo2Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txInInfo2Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV2.TxInInfo||])

txOut2Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txOut2Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV2.TxOut||])

-- txInfo2Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
-- txInfo2Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV2.TxInfo||])

-- scriptContext2Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
-- scriptContext2Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV2.ScriptContext||])
