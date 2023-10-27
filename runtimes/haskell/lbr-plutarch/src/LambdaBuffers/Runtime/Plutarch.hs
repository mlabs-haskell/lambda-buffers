{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Plutarch (PEitherData (..), PAssetClass, PMap, PChar, PSet, PValue) where

import Data.Functor.Const (Const)
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.TypeLits qualified as GHC
import Plutarch (
  ClosedTerm,
  PType,
  PlutusType (PInner),
  S,
  Term,
  pcon,
  pdelay,
  pforce,
  plam,
  pmatch,
  unTermCont,
  (#),
  type (:-->),
 )
import Plutarch.Api.V1 qualified
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PTokenName, PTuple)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList (PCons, PNil),
  PBuiltinPair,
  PData,
  PIsData (pdataImpl, pfromDataImpl),
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
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Lift (PUnsafeLiftDecl)
import Plutarch.List (
  PIsListLike,
  PList,
  PListLike (pcons, pnil),
  pfoldl,
 )
import Plutarch.Prelude (PEq ((#==)), PInteger, PPair (PPair), PTryFrom, pconstant, pif, ptrace, ptraceError, tcont)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.Unsafe (punsafeCoerce)

ptryFromData :: forall a s. PTryFrom PData a => Term s PData -> Term s a
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @a x)

pcasePlutusData ::
  Term s (PBuiltinPair PInteger (PBuiltinList PData) :--> a) ->
  Term s (PBuiltinList PData :--> a) ->
  Term s (PInteger :--> a) ->
  Term s (PData :--> a) ->
  Term s PData ->
  Term s a
pcasePlutusData handleConstr handleList handleInt handleOther pd =
  pforce $
    pchooseData
      # pd
      # pdelay (handleConstr # (pasConstr # pd))
      # pdelay (ptrace "Got a PlutusData Map" (handleOther # pd))
      # pdelay (handleList # (pasList # pd))
      # pdelay (handleInt # (pasInt # pd))
      # pdelay (ptrace "Got PlutusData Bytes" (handleOther # pd))

-- macro
lvListE :: PIsListLike list elem => [Term s elem] -> Term s (list elem)
lvListE = foldr (\x y -> pcons # x # y) pnil

lvIntE :: Integer -> Term s PInteger
lvIntE = pconstant

-- | `toPlutusData :: a -> PlutusData`
lvToPlutusData :: PIsData a => Term s a -> Term s PData
lvToPlutusData = pforgetData . pdata

-- | `constrData :: IntE -> ListE PlutusData -> PlutusData`
lvConstrToPlutusData :: PIsData a => Term s PInteger -> [Term s a] -> Term s PData
lvConstrToPlutusData ix args = pforgetData $ pconstrBuiltin # ix # lvListE (fmap lvToPlutusData args)

lvTupleE :: Term s a -> Term s b -> Term s (PPair a b)
lvTupleE l r = pcon (PPair l r)

pcaseConstr :: ClosedTerm (PBuiltinPair PInteger (PBuiltinList PData) :--> PList (PPair PInteger (PBuiltinList PData :--> a)) :--> a :--> a)
pcaseConstr = plam $ \pdConstr alts other -> unTermCont do
  ix <- pletC $ pfstBuiltin # pdConstr
  body <- pletC $ psndBuiltin # pdConstr
  pure $
    pfoldl
      # plam
        ( \res alt ->
            pmatch alt (\(PPair altIx altHandle) -> pif (ix #== altIx) (altHandle # body) res)
        )
      # other
      # alts

pcaseInt :: ClosedTerm (PInteger :--> PList (PPair PInteger a) :--> (PInteger :--> a) :--> a)
pcaseInt = plam $ \pdInt alts other -> unTermCont do
  intToVal <-
    pletC $
      pfoldl
        # plam
          ( \res alt ->
              pmatch alt (\(PPair altIx altValue) -> pif (pdInt #== altIx) (plam $ const altValue) res)
          )
        # other
        # alts
  pure $ intToVal # pdInt

data FooTrivial (s :: S) = FooTrivial

instance PlutusType FooTrivial where
  type PInner FooTrivial = PData
  pcon' FooTrivial = lvToPlutusData (lvIntE 0)
  pmatch' pd f =
    pcaseInt
      # (pasInt # pd)
      # lvListE [lvTupleE 0 (f FooTrivial)]
      # ptraceError "Got PlutusData Integer but invalid value"

instance PTryFrom PData FooTrivial where
  type PTryFromExcess PData FooTrivial = Const ()
  ptryFrom' pd f =
    pcasePlutusData
      (plam $ \_pdCons -> ptraceError "Got PlutusData Constr")
      (plam $ \_pdList -> ptraceError "Got PlutusData List")
      ( plam $ \pdInt ->
          pcaseInt
            # pdInt
            # lvListE [lvTupleE 0 (f (pcon FooTrivial, ()))]
            # ptraceError "Got PlutusData Integer but invalid value"
      )
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd

instance PIsData FooTrivial where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq FooTrivial where
  (#==) l r = pdata l #== pdata r

newtype FooLessTrivial (a :: PType) (s :: S) = FooLessTrivial (Term s a)

instance (PIsData a) => PlutusType (FooLessTrivial a) where
  type PInner (FooLessTrivial a) = PData
  pcon' (FooLessTrivial x) = lvConstrToPlutusData 0 [x]
  pmatch' pd f =
    pcaseConstr
      # (pasConstr # pd)
      # lvListE
        [ lvTupleE
            0
            ( plam $ \x1 ->
                pmatch
                  x1
                  ( \case
                      PCons x2 x3 ->
                        pmatch
                          x3
                          ( \case
                              PNil -> f (FooLessTrivial (punsafeCoerce x2))
                              _ -> ptraceError "err"
                          )
                      _ -> ptraceError "err"
                  )
            )
        ]
      # ptraceError "err"

--    pcasePlutusData
--   ( plam $ \pdConstr ->
--       pcaseConstr
--         # pdConstr
--         # ( lvListE
--               [ lvTupleE
--                   0
--                   ( plam $ \x1 ->
--                       pmatch
--                         x1
--                         ( \case
--                             PCons x2 x3 ->
--                               pmatch
--                                 x3
--                                 ( \case
--                                     PNil -> f (FooLessTrivial (punsafeCoerce x2))
--                                     _ -> ptraceError "err"
--                                 )
--                             _ -> ptraceError "err"
--                         )
--                   )
--               ]
--           )
--         # (ptraceError "err")
--   )
--   (plam $ \_pdList -> ptraceError "Got PlutusData List")
--   (plam $ \_pdInt -> ptraceError "Got PlutusData Integer")
--   (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
--   pd

instance (PTryFrom PData a, PIsData a) => PTryFrom PData (FooLessTrivial a) where
  type PTryFromExcess PData (FooLessTrivial a) = Const ()
  ptryFrom' pd f =
    pcasePlutusData
      ( plam $ \pdConstr ->
          pcaseConstr
            # pdConstr
            # lvListE
              [ lvTupleE
                  0
                  ( plam $ \x1 ->
                      pmatch
                        x1
                        ( \case
                            PCons x2 x3 ->
                              pmatch
                                x3
                                ( \case
                                    PNil -> f (pcon $ FooLessTrivial (ptryFromData x2), ())
                                    _ -> ptraceError "err"
                                )
                            _ -> ptraceError "err"
                        )
                  )
              ]
            # ptraceError "err"
      )
      (plam $ \_pdList -> ptraceError "Got PlutusData List")
      (plam $ \_pdInt -> ptraceError "Got PlutusData Integer")
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd

instance PIsData (FooLessTrivial a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (FooLessTrivial a) where
  (#==) l r = pdata l #== pdata r

data FooSum (a :: PType) (b :: PType) (s :: S)
  = FooSum'Bar (Term s a) (Term s (PMaybeData PAddress))
  | FooSum'Baz (Term s b) (Term s (PMaybeData PAssetClass))
  | FooSum'Bad
  | FooSum'Bax (Term s FooTrivial)

instance (PIsData a, PIsData b) => PIsData (FooSum a b)

instance (PTryFrom PData a, PTryFrom PData b, PIsData a, PIsData b) => PTryFrom PData (PAsData (FooSum a b)) where
  type PTryFromExcess PData (PAsData (FooSum a b)) = Const ()
  ptryFrom' pd f =
    pcasePlutusData
      ( plam $ \pdCons ->
          pcaseConstr
            # pdCons
            # ( pcons
                  # pcon
                    ( PPair
                        0
                        ( plam $ \x1 ->
                            pmatch
                              x1
                              ( \case
                                  PCons x2 x3 ->
                                    pmatch
                                      x3
                                      ( \case
                                          PCons x4 x5 ->
                                            pmatch
                                              x5
                                              ( \case
                                                  PNil -> f (pdata . pcon $ FooSum'Bar (ptryFromData x2) (ptryFromData x4), ())
                                                  _ -> ptraceError ""
                                              )
                                          _ -> ptraceError ""
                                      )
                                  _ -> ptraceError ""
                              )
                        )
                    )
                  # pnil
              )
            # ptraceError "Got PlutusData Constr but invalid constructor index value"
      )
      (plam $ \_pdList -> ptraceError "Got unexpected PlutusData List")
      (plam $ \pdInt -> pif (pdInt #== 2) (f (pdata $ pcon FooSum'Bad, ())) (ptraceError "Got PlutusData Integer but invalid value"))
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd

instance (PIsData a, PIsData b) => PlutusType (FooSum a b) where
  type PInner (FooSum a b) = PData
  pcon' (FooSum'Bar x y) = pforgetData $ pconstrBuiltin # 0 # (pcons # pforgetData (pdata x) # (pcons # pforgetData (pdata y) # pnil))
  pcon' (FooSum'Baz x y) = pforgetData $ pconstrBuiltin # 1 # (pcons # pforgetData (pdata x) # (pcons # pforgetData (pdata y) # pnil))
  pcon' FooSum'Bad = pforgetData $ pdata (2 :: Term s PInteger)
  pcon' (FooSum'Bax x) = pforgetData $ pconstrBuiltin # 3 # (pcons # pforgetData (pdata x) # pnil)
  pmatch' pd f =
    pcasePlutusData
      ( plam $ \pdCons ->
          pcaseConstr
            # pdCons
            # ( pcons
                  # pcon
                    ( PPair
                        0
                        ( plam $ \x1 ->
                            pmatch
                              x1
                              ( \case
                                  [x2, x3] -> f $ FooSum'Bar (punsafeCoerce $ pcon x2) (punsafeCoerce $ pcon x3)
                                  _ -> ptraceError ""
                              )
                        )
                    )
                  # pnil
              )
            # ptraceError "Got PlutusData Constr but invalid constructor index value"
      )
      (plam $ \_pdList -> ptraceError "Got unexpected PlutusData List")
      (plam $ \pdInt -> pif (pdInt #== 2) (f FooSum'Bad) (ptraceError "Got PlutusData Integer but invalid value"))
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd

instance (PIsData a, PUnsafeLiftDecl a) => IsList (Term s (PBuiltinList a)) where
  type Item (Term s (PBuiltinList a)) = Term s a
  fromList [] = pcon PNil
  fromList (x : xs) = pcon $ PCons x (fromList xs)
  toList = error "unimplemented"

instance (PIsData a, PlutusType a, PUnsafeLiftDecl a) => IsList (PBuiltinList a s) where
  type Item (PBuiltinList a s) = a s
  fromList [] = PNil
  fromList (x : xs) = PCons (pcon x) (fromList . fmap pcon $ xs)
  toList = error "unimplemented"

type PAssetClass = PTuple PCurrencySymbol PTokenName

data PEitherData (a :: PType) (b :: PType) (s :: S) = PDLeft (Term s a) | PDRight (Term s b)

instance (PIsData a, PIsData b) => PlutusType (PEitherData a b) where
  type PInner (PEitherData a b) = PData
  pcon' (PDLeft x) = lvConstrToPlutusData 0 [x]
  pcon' (PDRight x) = lvConstrToPlutusData 1 [x]
  pmatch' pd f =
    pcaseConstr
      # (pasConstr # pd)
      # lvListE
        [ lvTupleE
            0
            ( plam $ \x1 ->
                pmatch
                  x1
                  ( \case
                      PCons x2 x3 ->
                        pmatch
                          x3
                          ( \case
                              PNil -> f (PDLeft (punsafeCoerce x2))
                              _ -> ptraceError "err"
                          )
                      _ -> ptraceError "err"
                  )
            )
        , lvTupleE
            1
            ( plam $ \x1 ->
                pmatch
                  x1
                  ( \case
                      PCons x2 x3 ->
                        pmatch
                          x3
                          ( \case
                              PNil -> f (PDRight (punsafeCoerce x2))
                              _ -> ptraceError "err"
                          )
                      _ -> ptraceError "err"
                  )
            )
        ]
      # ptraceError "err"

instance (PTryFrom PData a, PIsData a, PTryFrom PData b, PIsData b) => PTryFrom PData (PEitherData a b) where
  type PTryFromExcess PData (PEitherData a b) = Const ()
  ptryFrom' pd f =
    pcasePlutusData
      ( plam $ \pdConstr ->
          pcaseConstr
            # pdConstr
            # lvListE
              [ lvTupleE
                  0
                  ( plam $ \x1 ->
                      pmatch
                        x1
                        ( \case
                            PCons x2 x3 ->
                              pmatch
                                x3
                                ( \case
                                    PNil -> f (pcon $ PDLeft (ptryFromData x2), ())
                                    _ -> ptraceError "err"
                                )
                            _ -> ptraceError "err"
                        )
                  )
              , lvTupleE
                  1
                  ( plam $ \x1 ->
                      pmatch
                        x1
                        ( \case
                            PCons x2 x3 ->
                              pmatch
                                x3
                                ( \case
                                    PNil -> f (pcon $ PDRight (ptryFromData x2), ())
                                    _ -> ptraceError "err"
                                )
                            _ -> ptraceError "err"
                        )
                  )
              ]
            # ptraceError "err"
      )
      (plam $ \_pdList -> ptraceError "Got PlutusData List")
      (plam $ \_pdInt -> ptraceError "Got PlutusData Integer")
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd

instance PIsData (PEitherData a b) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq (PEitherData a b) where
  (#==) l r = pdata l #== pdata r

type PMap = AssocMap.PMap 'AssocMap.Sorted

type PValue = Plutarch.Api.V1.PValue 'Plutarch.Api.V1.Sorted 'Plutarch.Api.V1.NoGuarantees

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
