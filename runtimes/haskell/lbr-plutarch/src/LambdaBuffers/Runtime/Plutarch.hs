{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Plutarch (PEither (..), PAssetClass, PMap, PChar, PSet, PValue, ptryFromPAsData, PMaybe (..), pcon) where

import Data.Functor.Const (Const)
import GHC.TypeLits qualified as GHC
import LambdaBuffers.Runtime.Plutarch.LamVal qualified as LamVal
import Plutarch (
  PType,
  PlutusType (PInner),
  S,
  Term,
  perror,
  pmatch,
  (#),
 )
import Plutarch.Api.V1 qualified
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PCurrencySymbol, PTokenName, PTuple)
import Plutarch.Builtin (
  PBuiltinList (PCons, PNil),
  PData,
  PIsData (pdataImpl, pfromDataImpl),
  pdata,
 )
import Plutarch.DataRepr.Internal ()
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Prelude (PAsData, PBool (PFalse, PTrue), PByteString, PEq ((#==)), PInteger, PTryFrom, pfromData, pif, ptryFrom)
import Plutarch.Prelude qualified as Pl
import Plutarch.Reducible (Reduce)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

-- | PAssetClass missing from Plutarch.
type PAssetClass = PTuple PCurrencySymbol PTokenName

-- | LB Plutus.Map maps to this, a sorted Plutus map.
type PMap = AssocMap.PMap 'AssocMap.Sorted

-- | LB Plutus.V1.Value maps to this, a sorted Value with no value guarantees.
type PValue = Plutarch.Api.V1.PValue 'Plutarch.Api.V1.Sorted 'Plutarch.Api.V1.NoGuarantees

-- | Not implemented.
data PChar (s :: S) = PChar

-- | PEither missing from Plutarch.
data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s (PAsData a))
  | PRight (Term s (PAsData b))

-- | PMaybe messed up in Plutarch so redefining here.
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s (PAsData a))
  | PNothing

data PFoo (a :: PType) (s :: S)
  = PFoo
      (Term s (PAsData PInteger))
      (Term s (PAsData PBool))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PMaybe a)))
      (Term s (PAsData (PEither a a)))
      (Term s (PAsData PAssetClass))
      (Term s (PAsData (PFoo a)))

-- PlutusType instances
-- Encodings: https://github.com/input-output-hk/plutus/blob/650a0659cbaacec2166e0153d2393c779cedc4c0/plutus-tx/src/PlutusTx/IsData/Instances.hs

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) = PData
  pcon' (PJust x) = LamVal.constrData 0 [LamVal.toPlutusData x]
  pcon' PNothing = LamVal.constrData 1 []
  pmatch' pd f =
    LamVal.casePlutusData
      ( \ix args ->
          pif
            (ix #== 0)
            ( pmatch args \case
                PNil -> perror
                PCons h t -> pif (t #== Pl.pcon PNil) (f $ PJust (LamVal.pfromPlutusDataPlutusType # h)) perror
            )
            ( pif
                (ix #== 1)
                ( pmatch args \case
                    PNil -> f PNothing
                    PCons _h _t -> perror
                )
                perror
            )
      )
      (const perror)
      (const perror)
      (const perror)
      pd

instance PlutusType (PEither a b) where
  type PInner (PEither a b) = PData
  pcon' (PLeft x) = LamVal.constrData 0 [LamVal.toPlutusData x]
  pcon' (PRight x) = LamVal.constrData 1 [LamVal.toPlutusData x]
  pmatch' pd f =
    LamVal.casePlutusData
      ( \ix args ->
          pif
            (ix #== 0)
            ( pmatch args \case
                PNil -> perror
                PCons h t -> pif (t #== Pl.pcon PNil) (f $ PLeft (LamVal.pfromPlutusDataPlutusType # h)) perror
            )
            ( pif
                (ix #== 1)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pif (t #== Pl.pcon PNil) (f $ PRight (LamVal.pfromPlutusDataPlutusType # h)) perror
                )
                perror
            )
      )
      (const perror)
      (const perror)
      (const perror)
      pd

instance PlutusType (PFoo a) where
  type PInner (PFoo a) = PData
  pcon' (PFoo i b bs may eit ac foo) =
    LamVal.listData
      [ LamVal.toPlutusData i
      , LamVal.toPlutusData b
      , LamVal.toPlutusData bs
      , LamVal.toPlutusData may
      , LamVal.toPlutusData eit
      , LamVal.toPlutusData ac
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

-- PTryFrom instances.

ptryFromPAsData :: forall a s r. (PTryFrom PData (PAsData a), PIsData a) => Term s PData -> ((Term s a, Reduce (PTryFromExcess PData (PAsData a) s)) -> Term s r) -> Term s r
ptryFromPAsData (pd :: Term s PData) f = ptryFrom @(PAsData a) pd (\(x, exc) -> f (pfromData x, exc))

instance (PTryFrom PData (PAsData a), PTryFrom PData (PAsData b)) => PTryFrom PData (PEither a b) where
  type PTryFromExcess PData (PEither a b) = Const ()
  ptryFrom' = ptryFromPAsData

instance (PTryFrom PData (PAsData a), PTryFrom PData (PAsData b)) => PTryFrom PData (PAsData (PEither a b)) where
  type PTryFromExcess PData (PAsData (PEither a b)) = Const ()
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
                        (t #== Pl.pcon PNil)
                        (pcon $ PLeft (LamVal.pfromPlutusDataPTryFrom # h))
                        perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons h t ->
                          pif
                            (t #== Pl.pcon PNil)
                            (pcon $ PRight (LamVal.pfromPlutusDataPTryFrom # h))
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

instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData

instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PMaybe a)) where
  type PTryFromExcess PData (PAsData (PMaybe a)) = Const ()
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
                        (t #== Pl.pcon PNil)
                        (pcon $ PJust (LamVal.pfromPlutusDataPTryFrom # h))
                        perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> pcon PNothing
                        PCons _h _t -> perror
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

instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PFoo a) where
  type PTryFromExcess PData (PFoo a) = Const ()
  ptryFrom' = ptryFromPAsData

instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PFoo a)) where
  type PTryFromExcess PData (PAsData (PFoo a)) = Const ()
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

{- | PTryFrom instance for PBool which is missing from Plutarch.
NOTE(bladyjoker): `PAsData PBool` here because its PInner is PBool for some god forsaken reason.
-}
instance PTryFrom PData (PAsData PBool) where
  type PTryFromExcess PData (PAsData PBool) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (args #== Pl.pcon PNil)
                ( pif
                    (ix #== 0)
                    (pcon PFalse)
                    ( pif
                        (ix #== 1)
                        (pcon PTrue)
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

instance PIsData (PFoo a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PIsData (PMaybe a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PIsData (PEither a b) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (PFoo a) where
  (#==) l r = pdata l #== pdata r

instance PEq (PMaybe a) where
  (#==) l r = pdata l #== pdata r

instance PEq (PEither a b) where
  (#==) l r = pdata l #== pdata r

pcon :: (PlutusType a, PIsData a) => a s -> Term s (PAsData a)
pcon = pdata . Pl.pcon
