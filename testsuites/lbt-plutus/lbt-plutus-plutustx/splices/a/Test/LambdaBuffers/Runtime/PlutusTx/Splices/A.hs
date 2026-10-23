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
  rationalCompiled,
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

rationalCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
rationalCompiled = $$(PlutusTx.compile [||fromToDataAndEq @PlutusTx.Ratio.Rational||])
