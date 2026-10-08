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

-- NOTE: splices are split across modules because the Plinth plugin keeps a
-- whole module's compiled scripts live; split a group further if it still OOMs.
module Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx.V1Tx (
  dcertCompiled,
  scriptPurposeCompiled,
  txInInfoCompiled,
  txOutCompiled,
  txInfoCompiled,
  scriptContextCompiled,
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
import Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx.Common (fromToData, fromToDataAndEq)

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
