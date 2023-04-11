module LambdaBuffers.Citizen (Citizen(..)
                             , CroatianOIB(..)
                             , Name(..)
                             , NationalId(..)
                             , Picture(..)
                             , ProdBar(..)
                             , ProdFoo(..)
                             , RecBar(..)
                             , RecFoo(..)
                             , SumFoo(..)
                             , SwissVisaType(..)) where

import LambdaBuffers.Plutus as LambdaBuffers.Plutus
import LambdaBuffers.Plutus.V1 as LambdaBuffers.Plutus.V1
import LambdaBuffers.Prelude as LambdaBuffers.Prelude
import Ctl.Internal.FromData as Ctl.Internal.FromData
import Ctl.Internal.ToData as Ctl.Internal.ToData
import Ctl.Internal.Types.PlutusData as Ctl.Internal.Types.PlutusData
import Data.BigInt as Data.BigInt
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Data.Tuple as Data.Tuple
import LambdaBuffers.Runtime.PlutusLedgerApi as LambdaBuffers.Runtime.PlutusLedgerApi
import Prelude as Prelude


newtype Citizen = Citizen { firstName :: Name
                          , lastName :: Name
                          , id :: NationalId}
derive instance Data.Newtype.Newtype Citizen _
derive instance Data.Generic.Rep.Generic Citizen _
instance Data.Show.Show Citizen where
  show = Data.Show.Generic.genericShow

newtype CroatianOIB = CroatianOIB LambdaBuffers.Prelude.Integer
derive instance Data.Newtype.Newtype CroatianOIB _
derive instance Data.Generic.Rep.Generic CroatianOIB _
instance Data.Show.Show CroatianOIB where
  show = Data.Show.Generic.genericShow

newtype Name = Name LambdaBuffers.Plutus.V1.Bytes
derive instance Data.Newtype.Newtype Name _
derive instance Data.Generic.Rep.Generic Name _
instance Data.Show.Show Name where
  show = Data.Show.Generic.genericShow

data NationalId = NationalId'CroatianPassport CroatianOIB Picture
                   | NationalId'CroatianIdCard CroatianOIB
                   | NationalId'SwissVisa SwissVisaType
derive instance Data.Generic.Rep.Generic NationalId _
instance Data.Show.Show NationalId where
  show = Data.Show.Generic.genericShow

newtype Picture = Picture LambdaBuffers.Plutus.V1.Bytes
derive instance Data.Newtype.Newtype Picture _
derive instance Data.Generic.Rep.Generic Picture _
instance Data.Show.Show Picture where
  show = Data.Show.Generic.genericShow

newtype ProdBar a = ProdBar a
derive instance Data.Newtype.Newtype (ProdBar a) _
derive instance Data.Generic.Rep.Generic (ProdBar a) _
instance (Data.Show.Show a) => Data.Show.Show (ProdBar a) where
  show = Data.Show.Generic.genericShow

data ProdFoo a = ProdFoo LambdaBuffers.Prelude.Text a
derive instance Data.Generic.Rep.Generic (ProdFoo a) _
instance (Data.Show.Show a) => Data.Show.Show (ProdFoo a) where
  show = Data.Show.Generic.genericShow

newtype RecBar a = RecBar { bar :: a}
derive instance Data.Newtype.Newtype (RecBar a) _
derive instance Data.Generic.Rep.Generic (RecBar a) _
instance (Data.Show.Show a) => Data.Show.Show (RecBar a) where
  show = Data.Show.Generic.genericShow

newtype RecFoo a = RecFoo { bar :: LambdaBuffers.Prelude.Text, baz :: a}
derive instance Data.Newtype.Newtype (RecFoo a) _
derive instance Data.Generic.Rep.Generic (RecFoo a) _
instance (Data.Show.Show a) => Data.Show.Show (RecFoo a) where
  show = Data.Show.Generic.genericShow

data SumFoo a b = SumFoo'Baz LambdaBuffers.Prelude.Text a
                   | SumFoo'Bar LambdaBuffers.Prelude.Integer b
derive instance Data.Generic.Rep.Generic (SumFoo a b) _
instance (Data.Show.Show a,Data.Show.Show b) => Data.Show.Show (SumFoo a
                                                                       b) where
  show = Data.Show.Generic.genericShow

data SwissVisaType = SwissVisaType'L  | SwissVisaType'B  | SwissVisaType'C 
derive instance Data.Generic.Rep.Generic SwissVisaType _
instance Data.Show.Show SwissVisaType where
  show = Data.Show.Generic.genericShow


instance Prelude.Eq Citizen where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).firstName) ((Data.Newtype.unwrap x1).firstName))) (Prelude.(==) ((Data.Newtype.unwrap x0).lastName) ((Data.Newtype.unwrap x1).lastName))) (Prelude.(==) ((Data.Newtype.unwrap x0).id) ((Data.Newtype.unwrap x1).id)) ) )

instance Ctl.Internal.ToData.ToData Citizen where
  toData = (\x0 -> Ctl.Internal.Types.PlutusData.List ([Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).firstName)
                                                        , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).lastName)
                                                        , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).id)]) )

instance Ctl.Internal.FromData.FromData Citizen where
  fromData = (\x0 -> LambdaBuffers.Runtime.PlutusLedgerApi.casePlutusData ((\x1 -> (\x2 -> Data.Maybe.Nothing ) )) ((\x3 -> case x3 of
                                                                                                                              [x4
                                                                                                                               , x5
                                                                                                                               , x6] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x4)) ((\x7 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x5)) ((\x8 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x6)) ((\x9 -> Data.Maybe.Just (Citizen { firstName : x7
                                                                                                                                                                                                                                                                                                                                                        , lastName : x8
                                                                                                                                                                                                                                                                                                                                                        , id : x9 }) )) )) ))
                                                                                                                              x10 -> Data.Maybe.Nothing )) ((\x11 -> Data.Maybe.Nothing )) ((\x12 -> Data.Maybe.Nothing )) (x0) )

instance Prelude.Eq Name where
  eq = (\x0 -> (\x1 -> let Name x2 = x0 in let Name x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Ctl.Internal.ToData.ToData Name where
  toData = (\x0 -> let Name x1 = x0 in Ctl.Internal.ToData.toData (x1) )

instance Ctl.Internal.FromData.FromData Name where
  fromData = (\x0 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x0)) ((\x1 -> Data.Maybe.Just (Name x1) )) )

instance Prelude.Eq NationalId where
  eq = (\x0 -> (\x1 -> case x0 of
                       NationalId'CroatianPassport x2 x3 -> case x1 of
                                                            NationalId'CroatianPassport x4 x5 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) (x2) (x4))) (Prelude.(==) (x3) (x5))
                                                            NationalId'CroatianIdCard x6 -> false
                                                            NationalId'SwissVisa x7 -> false
                       NationalId'CroatianIdCard x8 -> case x1 of
                                                       NationalId'CroatianPassport x9 x10 -> false
                                                       NationalId'CroatianIdCard x11 -> Prelude.(&&) (true) (Prelude.(==) (x8) (x11))
                                                       NationalId'SwissVisa x12 -> false
                       NationalId'SwissVisa x13 -> case x1 of
                                                   NationalId'CroatianPassport x14 x15 -> false
                                                   NationalId'CroatianIdCard x16 -> false
                                                   NationalId'SwissVisa x17 -> Prelude.(&&) (true) (Prelude.(==) (x13) (x17)) ) )

instance Ctl.Internal.ToData.ToData NationalId where
  toData = (\x0 -> case x0 of
                   NationalId'CroatianPassport x1 x2 -> LambdaBuffers.Runtime.PlutusLedgerApi.pdConstr ((Data.BigInt.fromInt 0)) ([Ctl.Internal.ToData.toData (x1)
                                                                                                                                   , Ctl.Internal.ToData.toData (x2)])
                   NationalId'CroatianIdCard x3 -> LambdaBuffers.Runtime.PlutusLedgerApi.pdConstr ((Data.BigInt.fromInt 1)) ([Ctl.Internal.ToData.toData (x3)])
                   NationalId'SwissVisa x4 -> LambdaBuffers.Runtime.PlutusLedgerApi.pdConstr ((Data.BigInt.fromInt 2)) ([Ctl.Internal.ToData.toData (x4)]) )

instance Ctl.Internal.FromData.FromData NationalId where
  fromData = (\x0 -> LambdaBuffers.Runtime.PlutusLedgerApi.casePlutusData ((\x1 -> (\x2 -> LambdaBuffers.Runtime.PlutusLedgerApi.caseInt [Data.Tuple.Tuple (Data.BigInt.fromInt 0) (case x2 of
                                                                                                                                                                                      [x3
                                                                                                                                                                                       , x4] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x3)) ((\x5 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x4)) ((\x6 -> Data.Maybe.Just (NationalId'CroatianPassport x5 x6) )) ))
                                                                                                                                                                                      x7 -> Data.Maybe.Nothing)
                                                                                                                                          , Data.Tuple.Tuple (Data.BigInt.fromInt 1) (case x2 of
                                                                                                                                                                                        [x8] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x8)) ((\x9 -> Data.Maybe.Just (NationalId'CroatianIdCard x9) ))
                                                                                                                                                                                        x10 -> Data.Maybe.Nothing)
                                                                                                                                          , Data.Tuple.Tuple (Data.BigInt.fromInt 2) (case x2 of
                                                                                                                                                                                        [x11] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x11)) ((\x12 -> Data.Maybe.Just (NationalId'SwissVisa x12) ))
                                                                                                                                                                                        x13 -> Data.Maybe.Nothing)] (\x14 -> Data.Maybe.Nothing ) x1 ) )) ((\x15 -> Data.Maybe.Nothing )) ((\x16 -> LambdaBuffers.Runtime.PlutusLedgerApi.caseInt [] (\x17 -> Data.Maybe.Nothing ) x16 )) ((\x18 -> Data.Maybe.Nothing )) (x0) )

instance Prelude.Eq CroatianOIB where
  eq = (\x0 -> (\x1 -> let CroatianOIB x2 = x0 in let CroatianOIB x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Ctl.Internal.ToData.ToData CroatianOIB where
  toData = (\x0 -> let CroatianOIB x1 = x0 in Ctl.Internal.ToData.toData (x1) )

instance Ctl.Internal.FromData.FromData CroatianOIB where
  fromData = (\x0 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x0)) ((\x1 -> Data.Maybe.Just (CroatianOIB x1) )) )

instance Prelude.Eq Picture where
  eq = (\x0 -> (\x1 -> let Picture x2 = x0 in let Picture x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Ctl.Internal.ToData.ToData Picture where
  toData = (\x0 -> let Picture x1 = x0 in Ctl.Internal.ToData.toData (x1) )

instance Ctl.Internal.FromData.FromData Picture where
  fromData = (\x0 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x0)) ((\x1 -> Data.Maybe.Just (Picture x1) )) )

instance Prelude.Eq SwissVisaType where
  eq = (\x0 -> (\x1 -> case x0 of
                       SwissVisaType'L -> case x1 of
                                          SwissVisaType'L -> true
                                          SwissVisaType'B -> false
                                          SwissVisaType'C -> false
                       SwissVisaType'B -> case x1 of
                                          SwissVisaType'L -> false
                                          SwissVisaType'B -> true
                                          SwissVisaType'C -> false
                       SwissVisaType'C -> case x1 of
                                          SwissVisaType'L -> false
                                          SwissVisaType'B -> false
                                          SwissVisaType'C -> true ) )

instance Ctl.Internal.ToData.ToData SwissVisaType where
  toData = (\x0 -> case x0 of
                   SwissVisaType'L -> Ctl.Internal.Types.PlutusData.Integer ((Data.BigInt.fromInt 0))
                   SwissVisaType'B -> Ctl.Internal.Types.PlutusData.Integer ((Data.BigInt.fromInt 1))
                   SwissVisaType'C -> Ctl.Internal.Types.PlutusData.Integer ((Data.BigInt.fromInt 2)) )

instance Ctl.Internal.FromData.FromData SwissVisaType where
  fromData = (\x0 -> LambdaBuffers.Runtime.PlutusLedgerApi.casePlutusData ((\x1 -> (\x2 -> LambdaBuffers.Runtime.PlutusLedgerApi.caseInt [] (\x3 -> Data.Maybe.Nothing ) x1 ) )) ((\x4 -> Data.Maybe.Nothing )) ((\x5 -> LambdaBuffers.Runtime.PlutusLedgerApi.caseInt [Data.Tuple.Tuple (Data.BigInt.fromInt 0) (Data.Maybe.Just (SwissVisaType'L))
                                                                                                                                                                                                                                                                        , Data.Tuple.Tuple (Data.BigInt.fromInt 1) (Data.Maybe.Just (SwissVisaType'B))
                                                                                                                                                                                                                                                                        , Data.Tuple.Tuple (Data.BigInt.fromInt 2) (Data.Maybe.Just (SwissVisaType'C))] (\x6 -> Data.Maybe.Nothing ) x5 )) ((\x7 -> Data.Maybe.Nothing )) (x0) )