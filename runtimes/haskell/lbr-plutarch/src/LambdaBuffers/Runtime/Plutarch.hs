{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Plutarch (PEitherData (..), PAssetClass, PMap, PChar, PSet, PValue, PInt) where

import Data.Functor.Const (Const)
import GHC.TypeLits qualified as GHC
import LambdaBuffers.Runtime.Plutarch.LamVal qualified as LamVal
import Plutarch (
  PType,
  PlutusType (PInner),
  S,
  Term,
  pcon,
  perror,
  pmatch,
  (#),
 )
import Plutarch.Api.V1 qualified
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PCurrencySymbol, PMaybeData, PTokenName, PTuple)
import Plutarch.Builtin (
  PBuiltinList (PCons, PNil),
  PData,
  PIsData (pdataImpl, pfromDataImpl),
  pdata,
 )
import Plutarch.DataRepr.Internal ()
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Prelude (PAsData, PBool (PFalse, PTrue), PByteString, PEq ((#==)), PInteger, PTryFrom, pif)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

type PInt = PAsData PInteger

-- | PAssetClass missing from Plutarch.
type PAssetClass = PTuple PCurrencySymbol PTokenName

-- | PEitherData missing from Plutarch.
data PEitherData (a :: PType) (b :: PType) (s :: S)
  = PDLeft (Term s (PAsData a))
  | PDRight (Term s (PAsData b))

instance PlutusType (PEitherData a b) where
  type PInner (PEitherData a b) = PData
  pcon' (PDLeft x) = LamVal.constrData 0 [LamVal.toPlutusData x]
  pcon' (PDRight x) = LamVal.constrData 1 [LamVal.toPlutusData x]
  pmatch' pd f =
    LamVal.casePlutusData
      ( \ix args ->
          pif
            (ix #== 0)
            ( pmatch args \case
                PNil -> perror
                PCons h t -> pif (t #== pcon PNil) (f $ PDLeft (LamVal.pfromPlutusDataPlutusType # h)) perror
            )
            ( pif
                (ix #== 1)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pif (t #== pcon PNil) (f $ PDRight (LamVal.pfromPlutusDataPlutusType # h)) perror
                )
                perror
            )
      )
      (const perror)
      (const perror)
      (const perror)
      pd

instance (PTryFrom PData (PAsData a), PTryFrom PData (PAsData b)) => PTryFrom PData (PEitherData a b) where
  type PTryFromExcess PData (PEitherData a b) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t ->
                      pif
                        (t #== pcon PNil)
                        (pcon $ PDLeft (LamVal.pfromPlutusDataPTryFrom # h))
                        perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons h t ->
                          pif
                            (t #== pcon PNil)
                            (pcon $ PDRight (LamVal.pfromPlutusDataPTryFrom # h))
                            perror
                    )
                    perror
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (PEitherData a b))

instance PIsData (PEitherData a b) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (PEitherData a b) where
  (#==) l r = pdata l #== pdata r

{- | PTryFrom instance for PBool which is missing from Plutarch.
https://github.com/input-output-hk/plutus/blob/650a0659cbaacec2166e0153d2393c779cedc4c0/plutus-tx/src/PlutusTx/IsData/Instances.hs

NOTE(bladyjoker): `PAsData PBool` here because its PInner is PBool for some god forsaken reason.
-}
instance PTryFrom PData (PAsData PBool) where
  type PTryFromExcess PData (PAsData PBool) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (args #== pcon PNil)
                ( pif
                    (ix #== 0)
                    (pdata $ pcon PFalse)
                    ( pif
                        (ix #== 1)
                        (pdata $ pcon PTrue)
                        perror
                    )
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

-- | LB Plutus.Map maps to this, a sorted Plutus map.
type PMap = AssocMap.PMap 'AssocMap.Sorted

-- | LB Plutus.V1.Value maps to this, a sorted Value with no value guarantees.
type PValue = Plutarch.Api.V1.PValue 'Plutarch.Api.V1.Sorted 'Plutarch.Api.V1.NoGuarantees

-- | Not implemented.
data PChar (s :: S) = PChar

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Char not implemented") => PlutusType PChar where
  type PInner PChar = PData
  pcon' PChar = error "unreachable"
  pmatch' _pd _f = error "unreachable"
instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Char not implemented") => PTryFrom PData PChar where
  type PTryFromExcess PData PChar = Const ()
  ptryFrom' _pd _f = error "unreachable"
instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Char not implemented") => PIsData PChar where
  pdataImpl = error "unreachable"
  pfromDataImpl = error "unreachable"

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Char not implemented") => PEq PChar where
  (#==) _l _r = error "unreachable"

-- | Not implemented.
data PSet (a :: PType) (s :: S) = PSet

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented") => PlutusType (PSet a) where
  type PInner (PSet a) = PData
  pcon' PSet = error "unreachable"
  pmatch' _pd _f = error "unreachable"
instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented") => PTryFrom PData (PSet a) where
  type PTryFromExcess PData (PSet a) = Const ()
  ptryFrom' _pd _f = error "unreachable"
instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented") => PIsData (PSet a) where
  pdataImpl = error "unreachable"
  pfromDataImpl = error "unreachable"
instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented") => PEq (PSet a) where
  (#==) _l _r = error "unreachable"

data PFoo (a :: PType) (s :: S)
  = PFoo
      (Term s (PAsData PInteger))
      (Term s (PAsData PBool))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PMaybeData a)))
      (Term s (PAsData (PEitherData a a)))
      (Term s (PAsData PCurrencySymbol))
      (Term s (PAsData (PFoo a)))

instance PlutusType (PFoo a) where
  type PInner (PFoo a) = PData
  pcon' (PFoo i b bs may eit sym foo) =
    LamVal.listData
      [ LamVal.toPlutusData i
      , LamVal.toPlutusData b
      , LamVal.toPlutusData bs
      , LamVal.toPlutusData may
      , LamVal.toPlutusData eit
      , LamVal.toPlutusData sym
      , LamVal.toPlutusData foo
      ]
  pmatch' pd f =
    f
      ( PFoo
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
          (LamVal.pfromPlutusDataPlutusType # pd)
      )

instance (PTryFrom PData a) => PTryFrom PData (PFoo a) where
  type PTryFromExcess PData (PFoo a) = Const ()
  ptryFrom' pd f =
    f
      ( pcon $
          PFoo
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
            (LamVal.pfromPlutusDataPTryFrom # pd)
      , ()
      )
instance PTryFrom PData (PAsData (PFoo a))

instance PIsData (PFoo a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (PFoo a) where
  (#==) l r = pdata l #== pdata r
