module LambdaBuffers.Runtime.Plutus.PlutusData (casePlutusData) where

import PlutusTx (BuiltinData)
import PlutusTx.Builtins (matchData)
import PlutusTx.Integer qualified

-- | LamVal `casePlutusData`
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
