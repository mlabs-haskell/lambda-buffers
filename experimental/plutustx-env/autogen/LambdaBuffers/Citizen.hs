module LambdaBuffers.Citizen (Citizen(..)
                             , CroatianOIB(..)
                             , Name(..)
                             , NationalId(..)
                             , Picture(..)
                             , SwissVisaType(..)) where

import qualified LambdaBuffers.Plutus
import qualified LambdaBuffers.Plutus.V1
import qualified LambdaBuffers.Prelude
import qualified PlutusTx
import qualified Prelude

data Citizen = MkCitizen { citizen'firstName :: Name
                         , citizen'lastName :: Name
                         , citizen'id :: NationalId}
newtype CroatianOIB = MkCroatianOIB LambdaBuffers.Prelude.Integer
newtype Name = MkName LambdaBuffers.Plutus.V1.Bytes
data NationalId = NationalId'CroatianPassport CroatianOIB Picture
                   | NationalId'CroatianIdCard CroatianOIB
                   | NationalId'SwissVisa SwissVisaType
newtype Picture = MkPicture LambdaBuffers.Plutus.V1.Bytes
data SwissVisaType = SwissVisaType'L  | SwissVisaType'B  | SwissVisaType'C 

instance Prelude.Eq Citizen where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (citizen'firstName x0)) (citizen'firstName x1)))) (((Prelude.==) (citizen'lastName x0)) (citizen'lastName x1)))) (((Prelude.==) (citizen'id x0)) (citizen'id x1))) ) )
instance PlutusTx.ToData Citizen where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> (PlutusTx.List [(PlutusTx.toData (citizen'firstName x0))
                                                     ,(PlutusTx.toData (citizen'lastName x0))
                                                     ,(PlutusTx.toData (citizen'id x0))]) )
instance Prelude.Eq Name where
  (==) = (\x0 -> (\x1 -> let MkName x2 = x0 in let MkName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance PlutusTx.ToData Name where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkName x1 = x0 in (PlutusTx.toData x1) )
instance Prelude.Eq NationalId where
  (==) = (\x0 -> (\x1 -> case x0 of
                         NationalId'CroatianPassport x2 x3 -> case x1 of
                                                              NationalId'CroatianPassport x4 x5 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x4))) (((Prelude.==) x3) x5))
                                                              NationalId'CroatianIdCard x6 -> Prelude.False
                                                              NationalId'SwissVisa x7 -> Prelude.False
                         NationalId'CroatianIdCard x8 -> case x1 of
                                                         NationalId'CroatianPassport x9 x10 -> Prelude.False
                                                         NationalId'CroatianIdCard x11 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x8) x11))
                                                         NationalId'SwissVisa x12 -> Prelude.False
                         NationalId'SwissVisa x13 -> case x1 of
                                                     NationalId'CroatianPassport x14 x15 -> Prelude.False
                                                     NationalId'CroatianIdCard x16 -> Prelude.False
                                                     NationalId'SwissVisa x17 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x13) x17)) ) )
instance PlutusTx.ToData NationalId where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> case x0 of
                                      NationalId'CroatianPassport x1 x2 -> ((PlutusTx.Constr 0) [(PlutusTx.toData x1)
                                                                                                ,(PlutusTx.toData x2)])
                                      NationalId'CroatianIdCard x3 -> ((PlutusTx.Constr 1) [(PlutusTx.toData x3)])
                                      NationalId'SwissVisa x4 -> ((PlutusTx.Constr 2) [(PlutusTx.toData x4)]) )
instance Prelude.Eq CroatianOIB where
  (==) = (\x0 -> (\x1 -> let MkCroatianOIB x2 = x0 in let MkCroatianOIB x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance PlutusTx.ToData CroatianOIB where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkCroatianOIB x1 = x0 in (PlutusTx.toData x1) )
instance Prelude.Eq Picture where
  (==) = (\x0 -> (\x1 -> let MkPicture x2 = x0 in let MkPicture x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance PlutusTx.ToData Picture where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkPicture x1 = x0 in (PlutusTx.toData x1) )
instance Prelude.Eq SwissVisaType where
  (==) = (\x0 -> (\x1 -> case x0 of
                         SwissVisaType'L  -> case x1 of
                                             SwissVisaType'L  -> Prelude.True
                                             SwissVisaType'B  -> Prelude.False
                                             SwissVisaType'C  -> Prelude.False
                         SwissVisaType'B  -> case x1 of
                                             SwissVisaType'L  -> Prelude.False
                                             SwissVisaType'B  -> Prelude.True
                                             SwissVisaType'C  -> Prelude.False
                         SwissVisaType'C  -> case x1 of
                                             SwissVisaType'L  -> Prelude.False
                                             SwissVisaType'B  -> Prelude.False
                                             SwissVisaType'C  -> Prelude.True ) )
instance PlutusTx.ToData SwissVisaType where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> case x0 of
                                      SwissVisaType'L  -> (PlutusTx.I 0)
                                      SwissVisaType'B  -> (PlutusTx.I 1)
                                      SwissVisaType'C  -> (PlutusTx.I 2) )