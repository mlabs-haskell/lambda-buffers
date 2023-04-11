module LambdaBuffers.Runtime.PlutusTx (casePlutusData) where

import PlutusTx.Builtins (BuiltinData, matchData)

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
