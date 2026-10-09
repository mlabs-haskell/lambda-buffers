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

module Test.LambdaBuffers.Runtime.PlutusTx.Splices.C (
  scriptPurposeV3Compiled,
  scriptInfoV3Compiled,
  txInInfoV3Compiled,
  txInfoV3Compiled,
  scriptContextV3Compiled,
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

scriptPurposeV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptPurposeV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.ScriptPurpose||])

scriptInfoV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptInfoV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.ScriptInfo||])

txInInfoV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txInInfoV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.TxInInfo||])

txInfoV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txInfoV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.TxInfo||])

scriptContextV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptContextV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.ScriptContext||])
