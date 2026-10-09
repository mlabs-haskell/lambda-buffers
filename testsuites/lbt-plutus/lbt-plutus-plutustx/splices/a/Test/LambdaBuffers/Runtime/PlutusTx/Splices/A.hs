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

module Test.LambdaBuffers.Runtime.PlutusTx.Splices.A (
  fromToDataAndEq,
  fromToData,
  integerCompiled,
  boolCompiled,
  dayCompiled,
  freeDayCompiled,
  workDayCompiled,
  fooACompiled,
  fooBCompiled,
  fooCCompiled,
  fooDCompiled,
  maybeCompiled,
  eitherCompiled,
  listCompiled,
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
  redeemerCompiled,
  redeemerHashCompiled,
  scriptHashCompiled,
  stakingCredentialCompiled,
  tokenNameCompiled,
  txIdCompiled,
  txOutRefCompiled,
  upperBoundCompiled,
  valueCompiled,
  rationalCompiled,
  txIdV3Compiled,
  txOutRefV3Compiled,
  coldCommitteeCredentialV3Compiled,
  hotCommitteeCredentialV3Compiled,
  drepCredentialV3Compiled,
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

{-# INLINEABLE fromToDataAndEq #-}
fromToDataAndEq :: forall a. (PlutusTx.Prelude.Eq a, FromData a, ToData a) => BuiltinData -> Bool
fromToDataAndEq x'data =
  let may'x = fromBuiltinData @a x'data
   in case may'x of
        Nothing -> trace "Failed FromData" (error ())
        Just x -> x == x && toBuiltinData x == x'data

{-# INLINEABLE fromToData #-}
fromToData :: forall a. (FromData a, ToData a) => BuiltinData -> Bool
fromToData x'data =
  let may'x = fromBuiltinData @a x'data
   in case may'x of
        Nothing -> trace "Failed FromData" (error ())
        Just x -> toBuiltinData x == x'data

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

maybeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
maybeCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(Maybe Bool)||])

eitherCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
eitherCompiled = $$(PlutusTx.compile [||fromToDataAndEq @(Either Bool Bool)||])

listCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
listCompiled = $$(PlutusTx.compile [||fromToDataAndEq @[Bool]||])

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

rationalCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
rationalCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusTx.Ratio.Rational||])

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
