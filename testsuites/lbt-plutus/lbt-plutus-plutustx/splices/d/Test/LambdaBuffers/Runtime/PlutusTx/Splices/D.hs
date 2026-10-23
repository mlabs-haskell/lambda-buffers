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

module Test.LambdaBuffers.Runtime.PlutusTx.Splices.D (
  drepV3Compiled,
  delegateeV3Compiled,
  txCertV3Compiled,
  voterV3Compiled,
  voteV3Compiled,
  governanceActionIdV3Compiled,
  committeeV3Compiled,
  constitutionV3Compiled,
  protocolVersionV3Compiled,
  changedParametersV3Compiled,
  governanceActionV3Compiled,
  proposalProcedureV3Compiled,
  dcertCompiled,
  scriptPurposeCompiled,
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

drepV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
drepV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.DRep||])

delegateeV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
delegateeV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.Delegatee||])

txCertV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
txCertV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.TxCert||])

voterV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
voterV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.Voter||])

voteV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
voteV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.Vote||])

governanceActionIdV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
governanceActionIdV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.GovernanceActionId||])

committeeV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
committeeV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.Committee||])

constitutionV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
constitutionV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.Constitution||])

protocolVersionV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
protocolVersionV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.ProtocolVersion||])

changedParametersV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
changedParametersV3Compiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV3.ChangedParameters||])

governanceActionV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
governanceActionV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.GovernanceAction||])

proposalProcedureV3Compiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
proposalProcedureV3Compiled = $$(PlutusTx.compile [||fromToData @PlutusV3.ProposalProcedure||])

dcertCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
dcertCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.DCert||])

scriptPurposeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
scriptPurposeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusV1.ScriptPurpose||])
