{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- TODO(bladyjoker): This module just tries to make sure things compile and it pprints the CompiledCode which is lame. Let's rather evaluate these scripts like proper people.
module Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx (tests) where

import LambdaBuffers.Days.PlutusTx (Day, FreeDay, WorkDay)
import LambdaBuffers.Foo.PlutusTx (A, B, C, D, E, FInt, GInt)
import PlutusTx (BuiltinData, CompiledCode, FromData (fromBuiltinData), ToData (toBuiltinData), compile, getPlc)
import PlutusTx.Maybe (fromMaybe)
import PlutusTx.Plugin ()
import PlutusTx.Prelude (Bool, Eq ((==)), Integer, traceError, (&&))
import Prettyprinter (Pretty (pretty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit (testCase)
import Prelude (IO, String, print, ($), (.))

{-# INLINEABLE fromToDataAndEq #-}
fromToDataAndEq :: forall a. (PlutusTx.Prelude.Eq a, FromData a, ToData a) => BuiltinData -> Bool
fromToDataAndEq x =
  let
    x' = fromMaybe (traceError "Failed parsing x from PlutusData") (fromBuiltinData @a x)
    x'' = toBuiltinData @a x'
   in
    x' == x' && x'' == x

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

fooFIntCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooFIntCompiled = $$(PlutusTx.compile [||fromToDataAndEq @FInt||])

fooGIntCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool)
fooGIntCompiled = $$(PlutusTx.compile [||fromToDataAndEq @GInt||])

tests :: TestTree
tests =
  testGroup
    "Just trying to compile with PlutusTx"
    [ testGroup
        "Compiling Prelude types"
        [ testCase "Prelude.Integer" (pprint integerCompiled)
        , testCase "Prelude.Bool" (pprint boolCompiled)
        , testCase "Days.Day" (pprint dayCompiled)
        , testCase "Days.FreeDay" (pprint freeDayCompiled)
        , testCase "Days.WorkDay" (pprint workDayCompiled)
        , testCase "Foo.A" (pprint fooACompiled)
        , testCase "Foo.B" (pprint fooBCompiled)
        , testCase "Foo.C" (pprint fooCCompiled)
        , testCase "Foo.D" (pprint fooDCompiled)
        , testCase "Foo.E" (pprint fooECompiled)
        , ignoreTestBecause "GHC Core to PLC plugin: E003:Error: Error from the PIR compiler: E003: Unsupported construct: Mutually recursive datatypes ((recursive) let binding; from [ AnnOther ])" $ testCase "Foo.FInt" (print ("Not printing" :: String))
        , ignoreTestBecause "GHC Core to PLC plugin: E003:Error: Error from the PIR compiler: E003: Unsupported construct: Mutually recursive datatypes ((recursive) let binding; from [ AnnOther ])" $ testCase "Foo.GInt" (print ("Not printing" :: String))
        ]
    ]
  where
    pprint :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> Bool) -> IO ()
    pprint = print . pretty . getPlc
