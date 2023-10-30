module LambdaBuffers.Runtime.Plutarch.LamVal (
  ptoPlutusData,
  pconstrData,
  pintegerData,
  pcasePlutusData,
  toPlutusData',
  constrData',
  integerData',
  casePlutusData',
  plistData,
  listData',
  psucceedParse,
  pfailParse,
  pbindParse,
  pfromPlutusData,
) where

import Plutarch (
  Term,
  pcon,
  pdelay,
  perror,
  pforce,
  plam,
  plet,
  (#),
  type (:-->),
 )
import Plutarch.Builtin (
  PBuiltinList (PNil),
  PData,
  PIsData,
  pasConstr,
  pasInt,
  pasList,
  pchooseData,
  pconstrBuiltin,
  pdata,
  pforgetData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Prelude (PBuiltinList (PCons), PInteger, ptrace)
import Plutarch.Unsafe (punsafeCoerce)

-- | `toPlutusData :: a -> PlutusData`
ptoPlutusData :: PIsData a => Term s (a :--> PData)
ptoPlutusData = plam toPlutusData'

-- | PlutusType's `toPlutusData :: a -> PlutusData`
toPlutusData' :: PIsData a => Term s a -> Term s PData
toPlutusData' = pforgetData . pdata

-- | `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusData :: Term s (PData :--> a)
pfromPlutusData = plam punsafeCoerce

{- | `constrData :: IntE -> ListE PlutusData -> PlutusData`
TODO(bladyjoker): Why PUnsafeLiftDecl
-}
pconstrData :: Term s (PInteger :--> PBuiltinList PData :--> PData)
pconstrData = plam $ \ix args -> pforgetData $ pconstrBuiltin # ix # args

-- | PlutusType's `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrData' :: Term s PInteger -> [Term s PData] -> Term s PData
constrData' ix args = pforgetData $ pconstrBuiltin # ix # toBuiltinList args

-- | `integerData :: IntE -> PlutusData`
pintegerData :: Term s (PInteger :--> PData)
pintegerData = ptoPlutusData

-- | PlutusType's `integerData :: IntE -> PlutusData`
integerData' :: Term s PInteger -> Term s PData
integerData' = toPlutusData'

-- | `listData :: ListE PlutusData -> PlutusData`
plistData :: Term s (PBuiltinList PData :--> PData)
plistData = plam $ pforgetData . pdata

-- | PlutusType's `listData :: ListE PlutusData -> PlutusData`
listData' :: [Term s PData] -> Term s PData
listData' = pforgetData . pdata . toBuiltinList

toBuiltinList :: [Term s PData] -> Term s (PBuiltinList PData)
toBuiltinList [] = pcon PNil
toBuiltinList (x : xs) = pcon (PCons x (toBuiltinList xs))

-- | `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
pcasePlutusData ::
  Term s ((PInteger :--> PBuiltinList PData :--> a) :--> (PBuiltinList PData :--> a) :--> (PInteger :--> a) :--> (PData :--> a) :--> PData :--> a)
pcasePlutusData = plam $ \handleConstr handleList handleInt handleOther pd ->
  pforce $
    pchooseData
      # pd
      # pdelay (plet (pasConstr # pd) $ \pair -> handleConstr # (pfstBuiltin # pair) # (psndBuiltin # pair))
      # pdelay (ptrace "Got a PlutusData Map" (handleOther # pd))
      # pdelay (handleList # (pasList # pd))
      # pdelay (handleInt # (pasInt # pd))
      # pdelay (ptrace "Got PlutusData Bytes" (handleOther # pd))

-- | PlutusType's `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
casePlutusData' ::
  (Term s PInteger -> Term s (PBuiltinList PData) -> Term s a) ->
  (Term s (PBuiltinList PData) -> Term s a) ->
  (Term s PInteger -> Term s a) ->
  (Term s PData -> Term s a) ->
  Term s PData ->
  Term s a
casePlutusData' handleConstr handleList handleInt handleOther pd = pcasePlutusData # plam handleConstr # plam handleList # plam handleInt # plam handleOther # pd

-- | `succeedParse :: a -> Parser a`
psucceedParse :: Term s (a :--> a)
psucceedParse = plam id

-- | `failParse :: Parser a`
pfailParse :: Term s a
pfailParse = perror

-- | `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
pbindParse :: Term s (a :--> (a :--> b) :--> b)
pbindParse = plam (flip (#))
