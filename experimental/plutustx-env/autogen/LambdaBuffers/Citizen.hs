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

import qualified LambdaBuffers.Plutus
import qualified LambdaBuffers.Plutus.V1
import qualified LambdaBuffers.Prelude
import qualified LambdaBuffers.Runtime.PlutusTx
import qualified PlutusTx
import qualified PlutusTx.Builtins
import qualified Prelude


data Citizen = Citizen { citizen'firstName :: Name
                       , citizen'lastName :: Name
                       , citizen'id :: NationalId} deriving Prelude.Show

newtype CroatianOIB = CroatianOIB LambdaBuffers.Prelude.Integer deriving Prelude.Show

newtype Name = Name LambdaBuffers.Plutus.V1.Bytes deriving Prelude.Show

data NationalId = NationalId'CroatianPassport CroatianOIB Picture
                   | NationalId'CroatianIdCard CroatianOIB
                   | NationalId'SwissVisa SwissVisaType deriving Prelude.Show

newtype Picture = Picture LambdaBuffers.Plutus.V1.Bytes deriving Prelude.Show

newtype ProdBar a = ProdBar a deriving Prelude.Show

data ProdFoo a = ProdFoo LambdaBuffers.Prelude.Text a deriving Prelude.Show

newtype RecBar a = RecBar { recBar'bar :: a} deriving Prelude.Show

data RecFoo a = RecFoo { recFoo'bar :: LambdaBuffers.Prelude.Text
                       , recFoo'baz :: a} deriving Prelude.Show

data SumFoo a b = SumFoo'Baz LambdaBuffers.Prelude.Text a
                   | SumFoo'Bar LambdaBuffers.Prelude.Integer
                                b deriving Prelude.Show

data SwissVisaType = SwissVisaType'L 
                      | SwissVisaType'B 
                      | SwissVisaType'C  deriving Prelude.Show


instance Prelude.Eq Citizen where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (citizen'firstName x0) (citizen'firstName x1))) ((Prelude.==) (citizen'lastName x0) (citizen'lastName x1))) ((Prelude.==) (citizen'id x0) (citizen'id x1)) ) )

instance PlutusTx.ToData Citizen where
  toBuiltinData = (\x0 -> PlutusTx.Builtins.mkList ([PlutusTx.toBuiltinData (citizen'firstName x0)
                                                     , PlutusTx.toBuiltinData (citizen'lastName x0)
                                                     , PlutusTx.toBuiltinData (citizen'id x0)]) )

instance PlutusTx.FromData Citizen where
  fromBuiltinData = (\x0 -> LambdaBuffers.Runtime.PlutusTx.casePlutusData ((\x1 -> (\x2 -> Prelude.Nothing ) )) ((\x3 -> case x3 of
                                                                                                                           [x4
                                                                                                                            , x5
                                                                                                                            , x6] -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x4)) ((\x7 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x5)) ((\x8 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x6)) ((\x9 -> Prelude.Just (Citizen { citizen'firstName = x7
                                                                                                                                                                                                                                                                                                                                , citizen'lastName = x8
                                                                                                                                                                                                                                                                                                                                , citizen'id = x9 }) )) )) ))
                                                                                                                           x10 -> Prelude.Nothing )) ((\x11 -> Prelude.Nothing )) ((\x12 -> Prelude.Nothing )) (x0) )

instance Prelude.Eq Name where
  (==) = (\x0 -> (\x1 -> let Name x2 = x0 in let Name x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance PlutusTx.ToData Name where
  toBuiltinData = (\x0 -> let Name x1 = x0 in PlutusTx.toBuiltinData (x1) )

instance PlutusTx.FromData Name where
  fromBuiltinData = (\x0 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x0)) ((\x1 -> Prelude.Just (Name x1) )) )

instance Prelude.Eq NationalId where
  (==) = (\x0 -> (\x1 -> case x0 of
                           NationalId'CroatianPassport x2 x3 -> case x1 of
                                                                  NationalId'CroatianPassport x4 x5 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x4))) ((Prelude.==) (x3) (x5))
                                                                  NationalId'CroatianIdCard x6 -> Prelude.False
                                                                  NationalId'SwissVisa x7 -> Prelude.False
                           NationalId'CroatianIdCard x8 -> case x1 of
                                                             NationalId'CroatianPassport x9 x10 -> Prelude.False
                                                             NationalId'CroatianIdCard x11 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x8) (x11))
                                                             NationalId'SwissVisa x12 -> Prelude.False
                           NationalId'SwissVisa x13 -> case x1 of
                                                         NationalId'CroatianPassport x14 x15 -> Prelude.False
                                                         NationalId'CroatianIdCard x16 -> Prelude.False
                                                         NationalId'SwissVisa x17 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x13) (x17)) ) )

instance PlutusTx.ToData NationalId where
  toBuiltinData = (\x0 -> case x0 of
                            NationalId'CroatianPassport x1 x2 -> PlutusTx.Builtins.mkConstr (0) ([PlutusTx.toBuiltinData (x1)
                                                                                                  , PlutusTx.toBuiltinData (x2)])
                            NationalId'CroatianIdCard x3 -> PlutusTx.Builtins.mkConstr (1) ([PlutusTx.toBuiltinData (x3)])
                            NationalId'SwissVisa x4 -> PlutusTx.Builtins.mkConstr (2) ([PlutusTx.toBuiltinData (x4)]) )

instance PlutusTx.FromData NationalId where
  fromBuiltinData = (\x0 -> LambdaBuffers.Runtime.PlutusTx.casePlutusData ((\x1 -> (\x2 -> case x1 of
                                                                                             0 -> case x2 of
                                                                                                    [x3
                                                                                                     , x4] -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x3)) ((\x5 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x4)) ((\x6 -> Prelude.Just (NationalId'CroatianPassport x5 x6) )) ))
                                                                                                    x7 -> Prelude.Nothing
                                                                                             1 -> case x2 of
                                                                                                    [x8] -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x8)) ((\x9 -> Prelude.Just (NationalId'CroatianIdCard x9) ))
                                                                                                    x10 -> Prelude.Nothing
                                                                                             2 -> case x2 of
                                                                                                    [x11] -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x11)) ((\x12 -> Prelude.Just (NationalId'SwissVisa x12) ))
                                                                                                    x13 -> Prelude.Nothing
                                                                                             x14 -> Prelude.Nothing ) )) ((\x15 -> Prelude.Nothing )) ((\x16 -> case x16 of
                                                                                                                                                                  x17 -> Prelude.Nothing )) ((\x18 -> Prelude.Nothing )) (x0) )

instance Prelude.Eq CroatianOIB where
  (==) = (\x0 -> (\x1 -> let CroatianOIB x2 = x0 in let CroatianOIB x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance PlutusTx.ToData CroatianOIB where
  toBuiltinData = (\x0 -> let CroatianOIB x1 = x0 in PlutusTx.toBuiltinData (x1) )

instance PlutusTx.FromData CroatianOIB where
  fromBuiltinData = (\x0 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x0)) ((\x1 -> Prelude.Just (CroatianOIB x1) )) )

instance Prelude.Eq Picture where
  (==) = (\x0 -> (\x1 -> let Picture x2 = x0 in let Picture x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance PlutusTx.ToData Picture where
  toBuiltinData = (\x0 -> let Picture x1 = x0 in PlutusTx.toBuiltinData (x1) )

instance PlutusTx.FromData Picture where
  fromBuiltinData = (\x0 -> (Prelude.>>=) (PlutusTx.fromBuiltinData (x0)) ((\x1 -> Prelude.Just (Picture x1) )) )

instance Prelude.Eq SwissVisaType where
  (==) = (\x0 -> (\x1 -> case x0 of
                           SwissVisaType'L -> case x1 of
                                                SwissVisaType'L -> Prelude.True
                                                SwissVisaType'B -> Prelude.False
                                                SwissVisaType'C -> Prelude.False
                           SwissVisaType'B -> case x1 of
                                                SwissVisaType'L -> Prelude.False
                                                SwissVisaType'B -> Prelude.True
                                                SwissVisaType'C -> Prelude.False
                           SwissVisaType'C -> case x1 of
                                                SwissVisaType'L -> Prelude.False
                                                SwissVisaType'B -> Prelude.False
                                                SwissVisaType'C -> Prelude.True ) )

instance PlutusTx.ToData SwissVisaType where
  toBuiltinData = (\x0 -> case x0 of
                            SwissVisaType'L -> PlutusTx.Builtins.mkI (0)
                            SwissVisaType'B -> PlutusTx.Builtins.mkI (1)
                            SwissVisaType'C -> PlutusTx.Builtins.mkI (2) )

instance PlutusTx.FromData SwissVisaType where
  fromBuiltinData = (\x0 -> LambdaBuffers.Runtime.PlutusTx.casePlutusData ((\x1 -> (\x2 -> case x1 of
                                                                                             x3 -> Prelude.Nothing ) )) ((\x4 -> Prelude.Nothing )) ((\x5 -> case x5 of
                                                                                                                                                               0 -> Prelude.Just (SwissVisaType'L)
                                                                                                                                                               1 -> Prelude.Just (SwissVisaType'B)
                                                                                                                                                               2 -> Prelude.Just (SwissVisaType'C)
                                                                                                                                                               x6 -> Prelude.Nothing )) ((\x7 -> Prelude.Nothing )) (x0) )