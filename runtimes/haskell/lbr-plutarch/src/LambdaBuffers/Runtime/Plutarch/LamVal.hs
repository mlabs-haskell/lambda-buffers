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

import Plutarch.Builtin.Data (
  PBuiltinList (PNil),
  PData,
  pasConstr,
  pasInt,
  pasList,
  pchooseData,
  pconstrBuiltin,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBuiltinList (PCons),
  PInteger,
  PTryFrom,
  Term,
  pcon,
  pdata,
  pdelay,
  perror,
  pforce,
  pforgetData,
  phoistAcyclic,
  plam,
  plet,
  ptraceInfo,
  ptryFrom,
  (#),
  type (:-->),
 )
import Plutarch.Unsafe (punsafeCoerce)

-- | Plutarch `toPlutusData :: a -> PlutusData`
ptoPlutusData :: ClosedTerm (PAsData a :--> PData)
ptoPlutusData = plam toPlutusData

-- | Haskell `toPlutusData :: a -> PlutusData`
toPlutusData :: Term s (PAsData a) -> Term s PData
toPlutusData = pforgetData

-- | Plutarch PlutusType `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPlutusType :: ClosedTerm (PData :--> PAsData a)
pfromPlutusDataPlutusType = plam punsafeCoerce

-- | Plutarch PTryFrom `fromPlutusData :: PlutusData -> Parser a`
pfromPlutusDataPTryFrom :: PTryFrom PData (PAsData a) => ClosedTerm (PData :--> PAsData a)
pfromPlutusDataPTryFrom = phoistAcyclic $ plam ptryFromData
  where
    ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
    ptryFromData pd = ptryFrom @(PAsData a) pd fst

-- | Plutarch `constrData :: IntE -> ListE PlutusData -> PlutusData`
pconstrData :: ClosedTerm (PInteger :--> PBuiltinList PData :--> PData)
pconstrData = phoistAcyclic $ plam $ \ix args -> pforgetData $ pconstrBuiltin # ix # args

-- | Haskell `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrData :: Term s PInteger -> [Term s PData] -> Term s PData
constrData ix args = pforgetData $ pconstrBuiltin # ix # toBuiltinList args

-- | Plutarch `integerData :: IntE -> PlutusData`
pintegerData :: ClosedTerm (PInteger :--> PData)
pintegerData = phoistAcyclic $ plam $ \i -> ptoPlutusData # pdata i

-- | Haskell `integerData :: IntE -> PlutusData`
integerData :: Term s PInteger -> Term s PData
integerData = toPlutusData . pdata

-- | Plutarch `listData :: ListE PlutusData -> PlutusData`
plistData :: ClosedTerm (PBuiltinList PData :--> PData)
plistData = phoistAcyclic $ plam $ pforgetData . pdata

-- | Haskell `listData :: ListE PlutusData -> PlutusData`
listData :: [Term s PData] -> Term s PData
listData = pforgetData . pdata . toBuiltinList

toBuiltinList :: [Term s PData] -> Term s (PBuiltinList PData)
toBuiltinList [] = pcon PNil
toBuiltinList (x : xs) = pcon (PCons x (toBuiltinList xs))

-- | Plutarch `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
pcasePlutusData ::
  ClosedTerm ((PInteger :--> PBuiltinList PData :--> a) :--> (PBuiltinList PData :--> a) :--> (PInteger :--> a) :--> (PData :--> a) :--> PData :--> a)
pcasePlutusData = phoistAcyclic $ plam $ \handleConstr handleList handleInt handleOther pd ->
  pforce $
    pchooseData
      # pd
      # pdelay (plet (pasConstr # pd) $ \pair -> handleConstr # (pfstBuiltin # pair) # (psndBuiltin # pair))
      # pdelay (ptraceInfo "Got a PlutusData Map" (handleOther # pd))
      # pdelay (handleList # (pasList # pd))
      # pdelay (handleInt # (pasInt # pd))
      # pdelay (ptraceInfo "Got PlutusData Bytes" (handleOther # pd))

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
psucceedParse :: ClosedTerm (a :--> a)
psucceedParse = plam id

-- | Plutarch `failParse :: Parser a`
pfailParse :: ClosedTerm a
pfailParse = perror

-- | Plutarch `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
pbindParse :: ClosedTerm (a :--> (a :--> b) :--> b)
pbindParse = phoistAcyclic $ plam (flip (#))
