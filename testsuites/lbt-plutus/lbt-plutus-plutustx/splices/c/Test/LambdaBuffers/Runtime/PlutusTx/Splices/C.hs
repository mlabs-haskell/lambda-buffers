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
  redeemerCompiled,
  redeemerHashCompiled,
  scriptHashCompiled,
  stakingCredentialCompiled,
  tokenNameCompiled,
  txIdCompiled,
  txOutRefCompiled,
  upperBoundCompiled,
  valueCompiled,
  txIdV3Compiled,
  txOutRefV3Compiled,
  coldCommitteeCredentialV3Compiled,
  hotCommitteeCredentialV3Compiled,
  drepCredentialV3Compiled,
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
import Test.LambdaBuffers.Runtime.PlutusTx.Splices.A (fromToDataAndEq)

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

txIdV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txIdV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.TxId||])

txOutRefV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txOutRefV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.TxOutRef||])

coldCommitteeCredentialV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
coldCommitteeCredentialV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.ColdCommitteeCredential||])

hotCommitteeCredentialV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
hotCommitteeCredentialV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.HotCommitteeCredential||])

drepCredentialV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
drepCredentialV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.DRepCredential||])
