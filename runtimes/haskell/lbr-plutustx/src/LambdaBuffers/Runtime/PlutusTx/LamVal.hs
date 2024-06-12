{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module LambdaBuffers.Runtime.PlutusTx.LamVal (caseIntE, casePlutusData) where

import PlutusTx (BuiltinData)
import PlutusTx.Base (const)
import PlutusTx.Builtins (matchData)
import PlutusTx.Eq (Eq ((==)))
import PlutusTx.Integer (Integer)

-- | LamVal `casePlutusData`
{-# INLINEABLE casePlutusData #-}
casePlutusData ::
  (PlutusTx.Integer.Integer -> [BuiltinData] -> r) ->
  ([BuiltinData] -> r) ->
  (PlutusTx.Integer.Integer -> r) ->
  (BuiltinData -> r) ->
  BuiltinData ->
  r
casePlutusData ctorCase listCase intCase otherCase pd =
  matchData
    pd
    ctorCase
    (const (otherCase pd))
    listCase
    intCase
    (const (otherCase pd))

{- | CaseIntE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
NOTE(bladyjoker): PlutusTx doesn't handle case expressions on Integer (see https://cardano.stackexchange.com/questions/8325/how-to-do-on-chain-integer-pattern-matching) as it tries the `base.Prelude` Integer's Eq instance rather than the PlutusTx one.
-}
{-# INLINEABLE caseIntE #-}
caseIntE :: Integer -> [(Integer, a)] -> (Integer -> a) -> a
caseIntE i [] other = other i
caseIntE i ((i', val) : cases) other = if i == i' then val else caseIntE i cases other
