{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module LambdaBuffers.Runtime.Plutarch (
  PEither (..),
  PPosixTimeRange,
  PMap,
  PChar,
  PSet,
  PType,
  PValue,
  ptryFromPAsData,
  PMaybe (..),
  pcon,
  PList (..),
  plistFrom,
) where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeLits qualified as GHC
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import LambdaBuffers.Runtime.Plutarch.LamVal qualified as LamVal
import Plutarch.DataRepr.Internal ()
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified
import Plutarch.LedgerApi.V1 qualified
import Plutarch.LedgerApi.V2 qualified
import Plutarch.LedgerApi.Value qualified
import Plutarch.Prelude (
  PAsData,
  PBool,
  PBuiltinList (PCons, PNil),
  PByteString,
  PData,
  PEq ((#==)),
  PInteger,
  PIsData (pdataImpl, pfromDataImpl),
  PListLike,
  PTryFrom (PTryFromExcess, ptryFrom'),
  PlutusType (PInner),
  S,
  Term,
  pasList,
  pdata,
  perror,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  pmatch,
  ptryFrom,
  (#),
 )
import Plutarch.Prelude qualified as Pl
import Plutarch.Reducible (Reduce)
import Plutarch.Unsafe (punsafeCoerce)

type PType = S -> Type

-- | LB Plutus.Map maps to this, a sorted Plutus map.
type PMap = AssocMap.PMap 'AssocMap.Sorted

-- | LB Plutus.V1.Value maps to this, a sorted Value with no value guarantees.
type PValue = Plutarch.LedgerApi.V1.PValue 'Plutarch.LedgerApi.V1.Sorted 'Plutarch.LedgerApi.V1.NoGuarantees

type PPosixTimeRange = Plutarch.LedgerApi.Interval.PInterval Plutarch.LedgerApi.V1.PPosixTime

-- | Not implemented.
data PChar (s :: S) = PChar

-- | Not implemented.
data PSet (a :: S -> Type) (s :: S) = PSet

-- | PEither missing from Plutarch.
data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s (PAsData a))
  | PRight (Term s (PAsData b))
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

-- | PMaybe messed up in Plutarch so redefining here.
data PMaybe (a :: S -> Type) (s :: S)
  = PJust (Term s (PAsData a))
  | PNothing
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

data PFoo (a :: S -> Type) (s :: S)
  = PFoo
      (Term s (PAsData (PMaybe PInteger)))
      (Term s (PAsData (PEither PBool PBool)))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PMaybe a)))
      (Term s (PAsData (PEither a a)))
      (Term s (PAsData Plutarch.LedgerApi.Value.PAssetClass))
      (Term s (PAsData (PFoo a)))
      (Term s (PAsData (PList a)))
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

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

-- instance PlutusType (PList a) where
--   type PInner (PList a) = (PBuiltinList (PAsData a))
--   pcon' (PList x) = x
--   pmatch' x f = f (PList x)

instance PlutusType (PFoo a) where
  type PInner (PFoo a) = PData
  pcon' (PFoo i b bs may eit ac foo xs) =
    LamVal.listData
      [ LamVal.toPlutusData i
      , LamVal.toPlutusData b
      , LamVal.toPlutusData bs
      , LamVal.toPlutusData may
      , LamVal.toPlutusData eit
      , LamVal.toPlutusData ac
      , LamVal.toPlutusData foo
      , LamVal.toPlutusData xs
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

instance PTryFrom PData (PAsData a) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PMaybe a)) where
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

instance PTryFrom PData (PAsData a) => PTryFrom PData (PFoo a) where
  type PTryFromExcess PData (PFoo a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData (PBuiltinList Plutarch.LedgerApi.V2.PTxOut)) where
  type PTryFromExcess PData (PAsData (PBuiltinList Plutarch.LedgerApi.V2.PTxOut)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.LedgerApi.V2.PTxOut # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (PBuiltinList Plutarch.LedgerApi.V1.PTxOut)) where
  type PTryFromExcess PData (PAsData (PBuiltinList Plutarch.LedgerApi.V1.PTxOut)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.LedgerApi.V1.PTxOut # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (PBuiltinList Plutarch.LedgerApi.V1.PDCert)) where
  type PTryFromExcess PData (PAsData (PBuiltinList Plutarch.LedgerApi.V1.PDCert)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.LedgerApi.V1.PDCert # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PFoo a)) where
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
            (LamVal.pfromPlutusDataPTryFrom # pd)
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

{- | PList because PBuiltinList misses `PAsData` on its constituents which causes type errors when used.
TODO(bladyjoker): Upstream these changes or fix PBuiltinList.
-}
newtype PList (a :: S -> Type) (s :: S)
  = PList (Term s (PBuiltinList (PAsData a)))
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

instance PlutusType (PList a) where
  type PInner (PList a) = PData
  pcon' (PList xs) = LamVal.toPlutusData $ pdata $ Pl.pmap # plam LamVal.toPlutusData # xs
  pmatch' pd f = f $ PList (punsafeCoerce $ pasList # pd)

instance PTryFrom PData (PAsData a) => PTryFrom PData (PList a) where
  type PTryFromExcess PData (PList a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PList a)) where
  type PTryFromExcess PData (PAsData (PList a)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs ->
              pcon $ PList $ Pl.pmap # plam (LamVal.pfromPlutusDataPTryFrom #) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PIsData (PList a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (PList a) where
  (#==) l r = Pl.pdata l #== Pl.pdata r

instance PListLike PList where
  type PElemConstraint PList a = PIsData a
  pelimList consCase nilCase ls = pmatch ls $ \case
    PList ls' -> pmatch ls' $ \case
      PCons x xs -> consCase (pfromData x) (Pl.pcon (PList xs))
      PNil -> nilCase
  pcons = phoistAcyclic $ plam $ \x xs -> pmatch xs (\(PList xs') -> Pl.pcon $ PList $ Pl.pcon (PCons (pdata x) xs'))
  pnil = Pl.pcon $ PList (Pl.pcon PNil)

plistFrom :: (PListLike l, Pl.PElemConstraint l a) => [Term s a] -> Term s (l a)
plistFrom = foldr (\x -> (#) (Pl.pcons # x)) Pl.pnil
