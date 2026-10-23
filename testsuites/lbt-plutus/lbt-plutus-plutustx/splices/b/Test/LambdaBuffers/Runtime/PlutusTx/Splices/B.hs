{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- Plinth splices, split across sublibraries (see lbt-plutus-plutustx.cabal).

module Test.LambdaBuffers.Runtime.PlutusTx.Splices.B (
  addressCompiled,
  assetClassCompiled,
  ledgerBytesCompiled,
  credentialCompiled,
  currencySymbolCompiled,
  datumCompiled,
  datumHashCompiled,
  extendedCompiled,
  intervalCompiled,
  lowerBoundCompiled,
  mapCompiled,
  posixTimeCompiled,
  posixTimeRangeCompiled,
  plutusDataCompiled,
)
where

import LambdaBuffers.Days.PlutusTx (Day, FreeDay, WorkDay)
import LambdaBuffers.Foo.PlutusTx (A, B, C, D)
import LambdaBuffers.Plutus.V2.PlutusTx qualified as PlutusV2
import LambdaBuffers.Plutus.V3.PlutusTx qualified as PlutusV3
import Plinth.Plugin ()
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusTx (BuiltinData, CompiledCode, FromData (fromBuiltinData), ToData (toBuiltinData), compile)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Maybe (Maybe (Just, Nothing))
import PlutusTx.Prelude (Bool, Either, Eq ((==)), Integer, error, trace, (&&))
import PlutusTx.Ratio qualified
import Test.LambdaBuffers.Runtime.PlutusTx.Splices.A (fromToData, fromToDataAndEq)

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

mapCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
mapCompiled = $$(PlutusTx.compile [||fromToData @(AssocMap.Map PlutusV1.CurrencySymbol (AssocMap.Map PlutusV1.TokenName Integer))||])

posixTimeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
posixTimeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.POSIXTime||])

posixTimeRangeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
posixTimeRangeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.POSIXTimeRange||])

plutusDataCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
plutusDataCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.BuiltinData||])
