module LambdaBuffers.Runtime.Plutarch.LamVal (
  ptoPlutusData,
  pconstrData,
  pintegerData,
  pcasePlutusData,
  toPlutusData,
  constrData,
  integerData,
  casePlutusData,
  plistData,
  listData,
  psucceedParse,
  pfailParse,
  pbindParse,
  pfromPlutusDataPlutusType,
  pfromPlutusDataPTryFrom,
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
import Plutarch.Prelude (PAsData, PBuiltinList (PCons), PInteger, PTryFrom, ptrace, ptryFrom)
import Plutarch.Unsafe (punsafeCoerce)

-- | Plutarch `toPlutusData :: a -> PlutusData`
ptoPlutusData :: Term s (PAsData a :--> PData)
ptoPlutusData = plam toPlutusData

-- | Haskell `toPlutusData :: a -> PlutusData`
toPlutusData :: Term s (PAsData a) -> Term s PData
toPlutusData = pforgetData

-- | Plutarch PlutusType `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPlutusType :: Term s (PData :--> PAsData a)
pfromPlutusDataPlutusType = plam punsafeCoerce

-- | Plutarch PTryFrom `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPTryFrom :: (PTryFrom PData (PAsData a)) => Term s (PData :--> PAsData a)
pfromPlutusDataPTryFrom = plam ptryFromData
  where
    ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
    ptryFromData pd = ptryFrom @(PAsData a) pd fst

-- | Plutarch `constrData :: IntE -> ListE PlutusData -> PlutusData`
pconstrData :: Term s (PInteger :--> PBuiltinList PData :--> PData)
pconstrData = plam $ \ix args -> pforgetData $ pconstrBuiltin # ix # args

-- | Haskell `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrData :: Term s PInteger -> [Term s PData] -> Term s PData
constrData ix args = pforgetData $ pconstrBuiltin # ix # toBuiltinList args

-- | Plutarch `integerData :: IntE -> PlutusData`
pintegerData :: Term s (PAsData PInteger :--> PData)
pintegerData = ptoPlutusData

-- | Haskell `integerData :: IntE -> PlutusData`
integerData :: Term s (PAsData PInteger) -> Term s PData
integerData = toPlutusData

-- | Plutarch `listData :: ListE PlutusData -> PlutusData`
plistData :: Term s (PBuiltinList PData :--> PData)
plistData = plam $ pforgetData . pdata

-- | Haskell `listData :: ListE PlutusData -> PlutusData`
listData :: [Term s PData] -> Term s PData
listData = pforgetData . pdata . toBuiltinList

toBuiltinList :: [Term s PData] -> Term s (PBuiltinList PData)
toBuiltinList [] = pcon PNil
toBuiltinList (x : xs) = pcon (PCons x (toBuiltinList xs))

-- | Plutarch `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
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

-- | Haskell `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
casePlutusData ::
  (Term s PInteger -> Term s (PBuiltinList PData) -> Term s a) ->
  (Term s (PBuiltinList PData) -> Term s a) ->
  (Term s PInteger -> Term s a) ->
  (Term s PData -> Term s a) ->
  Term s PData ->
  Term s a
casePlutusData handleConstr handleList handleInt handleOther pd = pcasePlutusData # plam handleConstr # plam handleList # plam handleInt # plam handleOther # pd

-- | Plutarch `succeedParse :: a -> Parser a`
psucceedParse :: Term s (a :--> a)
psucceedParse = plam id

-- | Plutarch `failParse :: Parser a`
pfailParse :: Term s a
pfailParse = perror

-- | Plutarch `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
pbindParse :: Term s (a :--> (a :--> b) :--> b)
pbindParse = plam (flip (#))
