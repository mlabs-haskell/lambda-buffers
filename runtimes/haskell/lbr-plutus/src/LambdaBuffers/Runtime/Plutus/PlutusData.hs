module LambdaBuffers.Runtime.Plutus.PlutusData (casePlutusData) where

import PlutusTx (BuiltinData)
import PlutusTx.Builtins (matchData)

-- | LamVal `casePlutusData`
casePlutusData ::
  (Integer -> [BuiltinData] -> r) ->
  ([BuiltinData] -> r) ->
  (Integer -> r) ->
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
