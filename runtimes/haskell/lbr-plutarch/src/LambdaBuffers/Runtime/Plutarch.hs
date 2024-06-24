{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module LambdaBuffers.Runtime.Plutarch (
  PEither (..),
  PAssetClass,
  PMap,
  PChar,
  PSet,
  PValue,
  ptryFromPAsData,
  PMaybe (..),
  pcon,
  PList (..),
  plistFrom,
) where

import Data.Functor.Const (Const)
import GHC.Generics (Generic)
import GHC.TypeLits qualified as GHC
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import LambdaBuffers.Runtime.Plutarch.LamVal qualified as LamVal
import Plutarch (
  PType,
  PlutusType (PInner),
  S,
  Term,
  perror,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
  (:-->),
 )
import Plutarch.Api.V1 (PMaybeData (PDJust, PDNothing))
import Plutarch.Api.V1 qualified
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.Api.V1.Scripts qualified
import Plutarch.Api.V2 qualified (POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash), PScriptContext (PScriptContext), PTxInInfo (PTxInInfo), PTxInfo (PTxInfo), PTxOut (PTxOut))
import Plutarch.Builtin (
  PBuiltinList (PCons, PNil),
  PData,
  PIsData (pdataImpl, pfromDataImpl),
  pasList,
  pdata,
 )
import Plutarch.Builtin qualified as Plutarch
import Plutarch.DataRepr.Internal ()
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Prelude (PAsData, PBool (PFalse, PTrue), PByteString, PEq ((#==)), PInteger, PListLike, PTryFrom, pdcons, pdnil, pfromData, pif, ptryFrom)
import Plutarch.Prelude qualified as Pl
import Plutarch.Reducible (Reduce)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

-- | PAssetClass missing from Plutarch.
type PAssetClass = Plutarch.Api.V1.PTuple Plutarch.Api.V1.PCurrencySymbol Plutarch.Api.V1.PTokenName

-- | LB Plutus.Map maps to this, a sorted Plutus map.
type PMap = AssocMap.PMap 'AssocMap.Sorted

-- | LB Plutus.V1.Value maps to this, a sorted Value with no value guarantees.
type PValue = Plutarch.Api.V1.PValue 'Plutarch.Api.V1.Sorted 'Plutarch.Api.V1.NoGuarantees

-- | Not implemented.
data PChar (s :: S) = PChar

-- | Not implemented.
data PSet (a :: PType) (s :: S) = PSet

-- | PEither missing from Plutarch.
data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s (PAsData a))
  | PRight (Term s (PAsData b))
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

-- | PMaybe messed up in Plutarch so redefining here.
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s (PAsData a))
  | PNothing
  deriving stock (Generic)
  deriving anyclass (Pl.PShow)

data PFoo (a :: PType) (s :: S)
  = PFoo
      (Term s (PAsData (PMaybe PInteger)))
      (Term s (PAsData (PEither PBool PBool)))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PMaybe a)))
      (Term s (PAsData (PEither a a)))
      (Term s (PAsData PAssetClass))
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

instance PTryFrom PData (PAsData Plutarch.Api.V1.Scripts.PDatum) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.Scripts.PDatum) = Const ()
  ptryFrom' pd f =
    f
      ( pcon $
          Plutarch.Api.V1.Scripts.PDatum
            (pfromData $ LamVal.pfromPlutusDataPTryFrom # pd)
      , ()
      )

instance PTryFrom PData Plutarch.Api.V1.Scripts.PDatum where
  type PTryFromExcess PData Plutarch.Api.V1.Scripts.PDatum = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData Plutarch.Api.V1.Scripts.PRedeemer) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.Scripts.PRedeemer) = Const ()
  ptryFrom' pd f =
    f
      ( pcon $
          Plutarch.Api.V1.Scripts.PRedeemer
            (pfromData $ LamVal.pfromPlutusDataPTryFrom # pd)
      , ()
      )

instance PTryFrom PData Plutarch.Api.V1.Scripts.PRedeemer where
  type PTryFromExcess PData Plutarch.Api.V1.Scripts.PRedeemer = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData Plutarch.Api.V1.Scripts.PRedeemerHash) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.Scripts.PRedeemerHash) = Const ()
  ptryFrom' pd f =
    f
      ( pcon $
          Plutarch.Api.V1.Scripts.PRedeemerHash
            (pfromData $ LamVal.pfromPlutusDataPTryFrom # pd)
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.Scripts.PDatumHash) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.Scripts.PDatumHash) = Const ()
  ptryFrom' pd f =
    f
      ( pcon $
          Plutarch.Api.V1.Scripts.PDatumHash
            (pfromData $ LamVal.pfromPlutusDataPTryFrom # pd)
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (Plutarch.Api.V1.PInterval a)) where
  type PTryFromExcess PData (PAsData (Plutarch.Api.V1.PInterval a)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V1.PInterval (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (Plutarch.Api.V1.PInterval a) where
  type PTryFromExcess PData (Plutarch.Api.V1.PInterval a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (Plutarch.Api.V1.PLowerBound a)) where
  type PTryFromExcess PData (PAsData (Plutarch.Api.V1.PLowerBound a)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V1.PLowerBound (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (Plutarch.Api.V1.PLowerBound a) where
  type PTryFromExcess PData (Plutarch.Api.V1.PLowerBound a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (Plutarch.Api.V1.PUpperBound a)) where
  type PTryFromExcess PData (PAsData (Plutarch.Api.V1.PUpperBound a)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V1.PUpperBound (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (Plutarch.Api.V1.PUpperBound a) where
  type PTryFromExcess PData (Plutarch.Api.V1.PUpperBound a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (Plutarch.Api.V1.PExtended a)) where
  type PTryFromExcess PData (PAsData (Plutarch.Api.V1.PExtended a)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> pcon $ Plutarch.Api.V1.PNegInf pdnil
                    _other -> perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons h t -> pmatch t \case
                          PNil -> pcon $ Plutarch.Api.V1.PFinite (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # pdnil)
                          _other -> perror
                    )
                    ( pif
                        (ix #== 2)
                        ( pmatch args \case
                            PNil -> pcon $ Plutarch.Api.V1.PPosInf pdnil
                            _other -> perror
                        )
                        perror
                    )
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData a) => PTryFrom PData (Plutarch.Api.V1.PExtended a) where
  type PTryFromExcess PData (Plutarch.Api.V1.PExtended a) = Const ()
  ptryFrom' = ptryFromPAsData

instance PTryFrom PData (PAsData Plutarch.Api.V2.POutputDatum) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V2.POutputDatum) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> pcon $ Plutarch.Api.V2.PNoOutputDatum pdnil
                    _other -> perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons h t -> pmatch t \case
                          PNil -> pcon $ Plutarch.Api.V2.POutputDatumHash (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # pdnil)
                          _other -> perror
                    )
                    ( pif
                        (ix #== 2)
                        ( pmatch args \case
                            PNil -> perror
                            PCons h t -> pmatch t \case
                              PNil -> pcon $ Plutarch.Api.V2.POutputDatum (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # pdnil)
                              _other -> perror
                        )
                        perror
                    )
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V2.PTxOut) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V2.PTxOut) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> perror
                        PCons h'' t'' -> pmatch t'' \case
                          PNil -> perror
                          PCons h''' t''' -> pmatch t''' \case
                            PNil ->
                              pcon $
                                Plutarch.Api.V2.PTxOut
                                  ( pdcons
                                      # (LamVal.pfromPlutusDataPTryFrom # h)
                                      # ( pdcons
                                            # (LamVal.pfromPlutusDataPTryFrom # h')
                                            # ( pdcons
                                                  # (LamVal.pfromPlutusDataPTryFrom # h'')
                                                  # ( pdcons
                                                        # (maybeToMaybe # (LamVal.pfromPlutusDataPTryFrom @(PMaybe PScriptHash) # h'''))
                                                        # pdnil
                                                    )
                                              )
                                        )
                                  )
                            _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PTxOut) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PTxOut) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons address t -> pmatch t \case
                      PNil -> perror
                      PCons val t1 -> pmatch t1 \case
                        PNil -> perror
                        PCons mayDatumHash t2 -> pmatch t2 \case
                          PNil ->
                            pcon $
                              Plutarch.Api.V1.PTxOut
                                ( pdcons
                                    # (LamVal.pfromPlutusDataPTryFrom # address)
                                    # ( pdcons
                                          # (LamVal.pfromPlutusDataPTryFrom # val)
                                          # ( pdcons
                                                # (maybeToMaybe # (LamVal.pfromPlutusDataPTryFrom # mayDatumHash))
                                                # pdnil
                                            )
                                      )
                                )
                          _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

-- HACK(bladyjoker): This is used above and it's a hack because something is off with PMaybeData instances.
maybeToMaybe :: Term s (PAsData (PMaybe a) :--> PAsData (PMaybeData a))
maybeToMaybe =
  phoistAcyclic $
    plam
      ( \may -> pmatch (pfromData may) $ \case
          PJust x -> pcon $ PDJust (pdcons # x # pdnil)
          PNothing -> pcon $ PDNothing pdnil
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PScriptContext) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PScriptContext) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V1.PScriptContext (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V2.PScriptContext) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V2.PScriptContext) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V2.PScriptContext (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PTxInInfo) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PTxInInfo) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V1.PTxInInfo (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V2.PTxInInfo) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V2.PTxInInfo) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons h t -> pmatch t \case
                      PNil -> perror
                      PCons h' t' -> pmatch t' \case
                        PNil -> pcon $ Plutarch.Api.V2.PTxInInfo (pdcons # (LamVal.pfromPlutusDataPTryFrom # h) # (pdcons # (LamVal.pfromPlutusDataPTryFrom # h') # pdnil))
                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V2.PTxInInfo)) where
  type PTryFromExcess PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V2.PTxInInfo)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.Api.V2.PTxInInfo # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V2.PTxOut)) where
  type PTryFromExcess PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V2.PTxOut)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.Api.V2.PTxOut # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PTxInInfo)) where
  type PTryFromExcess PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PTxInInfo)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.Api.V1.PTxInInfo # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PTxOut)) where
  type PTryFromExcess PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PTxOut)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.Api.V1.PTxOut # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PDCert) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PDCert) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons stakingCred args2 -> pmatch args2 \case
                      PNil -> pcon $ Plutarch.Api.V1.PDCertDelegRegKey (pdcons # (LamVal.pfromPlutusDataPTryFrom # stakingCred) # pdnil)
                      _other -> perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons stakingCred args2 -> pmatch args2 \case
                          PNil -> pcon $ Plutarch.Api.V1.PDCertDelegDeRegKey (pdcons # (LamVal.pfromPlutusDataPTryFrom # stakingCred) # pdnil)
                          _other -> perror
                    )
                    ( pif
                        (ix #== 2)
                        ( pmatch args \case
                            PNil -> perror
                            PCons stakingCred args2 -> pmatch args2 \case
                              PNil -> perror
                              PCons pkh args3 -> pmatch args3 \case
                                PNil ->
                                  pcon $
                                    Plutarch.Api.V1.PDCertDelegDelegate
                                      ( pdcons
                                          # (LamVal.pfromPlutusDataPTryFrom # stakingCred)
                                          # ( pdcons
                                                # (LamVal.pfromPlutusDataPTryFrom # pkh)
                                                # pdnil
                                            )
                                      )
                                _other -> perror
                        )
                        ( pif
                            (ix #== 3)
                            ( pmatch args \case
                                PNil -> perror
                                PCons pkh1 args2 -> pmatch args2 \case
                                  PNil -> perror
                                  PCons pkh2 args3 -> pmatch args3 \case
                                    PNil ->
                                      pcon $
                                        Plutarch.Api.V1.PDCertPoolRegister
                                          ( pdcons
                                              # (LamVal.pfromPlutusDataPTryFrom # pkh1)
                                              # ( pdcons
                                                    # (LamVal.pfromPlutusDataPTryFrom # pkh2)
                                                    # pdnil
                                                )
                                          )
                                    _other -> perror
                            )
                            ( pif
                                (ix #== 4)
                                ( pmatch args \case
                                    PNil -> perror
                                    PCons pkh args2 -> pmatch args2 \case
                                      PNil -> perror
                                      PCons int args3 -> pmatch args3 \case
                                        PNil ->
                                          pcon $
                                            Plutarch.Api.V1.PDCertPoolRetire
                                              ( pdcons
                                                  # (LamVal.pfromPlutusDataPTryFrom # pkh)
                                                  # ( pdcons
                                                        # (LamVal.pfromPlutusDataPTryFrom # int)
                                                        # pdnil
                                                    )
                                              )
                                        _other -> perror
                                )
                                ( pif
                                    (ix #== 5)
                                    ( pmatch args \case
                                        PNil -> pcon $ Plutarch.Api.V1.PDCertGenesis pdnil
                                        _other -> perror
                                    )
                                    ( pif
                                        (ix #== 6)
                                        ( pmatch args \case
                                            PNil -> pcon $ Plutarch.Api.V1.PDCertMir pdnil
                                            _other -> perror
                                        )
                                        perror
                                    )
                                )
                            )
                        )
                    )
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PScriptPurpose) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PScriptPurpose) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons symbol args2 -> pmatch args2 \case
                      PNil -> pcon $ Plutarch.Api.V1.PMinting (pdcons # (LamVal.pfromPlutusDataPTryFrom # symbol) # pdnil)
                      _other -> perror
                )
                ( pif
                    (ix #== 1)
                    ( pmatch args \case
                        PNil -> perror
                        PCons txOutRef args2 -> pmatch args2 \case
                          PNil -> pcon $ Plutarch.Api.V1.PSpending (pdcons # (LamVal.pfromPlutusDataPTryFrom # txOutRef) # pdnil)
                          _other -> perror
                    )
                    ( pif
                        (ix #== 2)
                        ( pmatch args \case
                            PNil -> perror
                            PCons stakingCred args2 -> pmatch args2 \case
                              PNil -> pcon $ Plutarch.Api.V1.PRewarding (pdcons # (LamVal.pfromPlutusDataPTryFrom # stakingCred) # pdnil)
                              _other -> perror
                        )
                        ( pif
                            (ix #== 3)
                            ( pmatch args \case
                                PNil -> perror
                                PCons dcert args2 -> pmatch args2 \case
                                  PNil -> pcon $ Plutarch.Api.V1.PCertifying (pdcons # (LamVal.pfromPlutusDataPTryFrom # dcert) # pdnil)
                                  _other -> perror
                            )
                            perror
                        )
                    )
                )
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PDCert)) where
  type PTryFromExcess PData (PAsData (Plutarch.PBuiltinList Plutarch.Api.V1.PDCert)) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          (const $ const perror)
          ( \xs -> pdata $ Pl.pmap # plam (\xpd -> pfromData $ pfromPlutusDataPTryFrom @Plutarch.Api.V1.PDCert # xpd) # xs
          )
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V2.PTxInfo) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V2.PTxInfo) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons inputs t -> pmatch t \case
                      PNil -> perror
                      PCons references t1 -> pmatch t1 \case
                        PNil -> perror
                        PCons outputs t2 -> pmatch t2 \case
                          PNil -> perror
                          PCons fee t3 -> pmatch t3 \case
                            PNil -> perror
                            PCons mint t4 -> pmatch t4 \case
                              PNil -> perror
                              PCons dcert t5 -> pmatch t5 \case
                                PNil -> perror
                                PCons wdrl t6 -> pmatch t6 \case
                                  PNil -> perror
                                  PCons range t7 -> pmatch t7 \case
                                    PNil -> perror
                                    PCons sigs t8 -> pmatch t8 \case
                                      PNil -> perror
                                      PCons redeemers t9 -> pmatch t9 \case
                                        PNil -> perror
                                        PCons datums t10 -> pmatch t10 \case
                                          PNil -> perror
                                          PCons txId t11 -> pmatch t11 \case
                                            PNil ->
                                              pcon $
                                                Plutarch.Api.V2.PTxInfo
                                                  ( pdcons
                                                      # (LamVal.pfromPlutusDataPTryFrom # inputs)
                                                      # ( pdcons
                                                            # (LamVal.pfromPlutusDataPTryFrom # references)
                                                            # ( pdcons
                                                                  # (LamVal.pfromPlutusDataPTryFrom # outputs)
                                                                  # ( pdcons
                                                                        # (LamVal.pfromPlutusDataPTryFrom # fee)
                                                                        # ( pdcons
                                                                              # (LamVal.pfromPlutusDataPTryFrom # mint)
                                                                              # ( pdcons
                                                                                    # (LamVal.pfromPlutusDataPTryFrom # dcert)
                                                                                    # ( pdcons
                                                                                          # (LamVal.pfromPlutusDataPTryFrom # wdrl)
                                                                                          # ( pdcons
                                                                                                # (LamVal.pfromPlutusDataPTryFrom # range)
                                                                                                # ( pdcons
                                                                                                      # (LamVal.pfromPlutusDataPTryFrom # sigs)
                                                                                                      # ( pdcons
                                                                                                            # (LamVal.pfromPlutusDataPTryFrom # redeemers)
                                                                                                            # ( pdcons
                                                                                                                  # (LamVal.pfromPlutusDataPTryFrom # datums)
                                                                                                                  # ( pdcons
                                                                                                                        # (LamVal.pfromPlutusDataPTryFrom # txId)
                                                                                                                        # pdnil
                                                                                                                    )
                                                                                                              )
                                                                                                        )
                                                                                                  )
                                                                                            )
                                                                                      )
                                                                                )
                                                                          )
                                                                    )
                                                              )
                                                        )
                                                  )
                                            _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
      , ()
      )

instance PTryFrom PData (PAsData Plutarch.Api.V1.PTxInfo) where
  type PTryFromExcess PData (PAsData Plutarch.Api.V1.PTxInfo) = Const ()
  ptryFrom' pd f =
    f
      ( LamVal.casePlutusData
          ( \ix args ->
              pif
                (ix #== 0)
                ( pmatch args \case
                    PNil -> perror
                    PCons inputs t -> pmatch t \case
                      PNil -> perror
                      PCons outputs t1 -> pmatch t1 \case
                        PNil -> perror
                        PCons fee t2 -> pmatch t2 \case
                          PNil -> perror
                          PCons mint t3 -> pmatch t3 \case
                            PNil -> perror
                            PCons dcert t4 -> pmatch t4 \case
                              PNil -> perror
                              PCons wdrl t5 -> pmatch t5 \case
                                PNil -> perror
                                PCons range t6 -> pmatch t6 \case
                                  PNil -> perror
                                  PCons sigs t7 -> pmatch t7 \case
                                    PNil -> perror
                                    PCons datums t8 -> pmatch t8 \case
                                      PNil -> perror
                                      PCons txId t9 -> pmatch t9 \case
                                        PNil ->
                                          pcon $
                                            Plutarch.Api.V1.PTxInfo
                                              ( pdcons
                                                  # (LamVal.pfromPlutusDataPTryFrom # inputs)
                                                  # ( pdcons
                                                        # (LamVal.pfromPlutusDataPTryFrom # outputs)
                                                        # ( pdcons
                                                              # (LamVal.pfromPlutusDataPTryFrom # fee)
                                                              # ( pdcons
                                                                    # (LamVal.pfromPlutusDataPTryFrom # mint)
                                                                    # ( pdcons
                                                                          # (LamVal.pfromPlutusDataPTryFrom # dcert)
                                                                          # ( pdcons
                                                                                # (LamVal.pfromPlutusDataPTryFrom # wdrl)
                                                                                # ( pdcons
                                                                                      # (LamVal.pfromPlutusDataPTryFrom # range)
                                                                                      # ( pdcons
                                                                                            # (LamVal.pfromPlutusDataPTryFrom # sigs)
                                                                                            # ( pdcons
                                                                                                  # (LamVal.pfromPlutusDataPTryFrom # datums)
                                                                                                  # ( pdcons
                                                                                                        # (LamVal.pfromPlutusDataPTryFrom # txId)
                                                                                                        # pdnil
                                                                                                    )
                                                                                              )
                                                                                        )
                                                                                  )
                                                                            )
                                                                      )
                                                                )
                                                          )
                                                    )
                                              )
                                        _other -> perror
                )
                perror
          )
          (const perror)
          (const perror)
          (const perror)
          pd
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
newtype PList (a :: PType) (s :: S)
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
