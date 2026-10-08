{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx.Common (fromToDataAndEq, fromToData) where

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
