{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Compiler.ProtoCompat.SILEq (
  (=:=),
  SILEq (sileq),
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Generics.SOP (All2, Generic (..), Proxy (..), hcmap, mapII)

{- | SourceInfo Less Equality.
 A TypeClass that enables equality without taking into consideration the SourceInfo inside a datatype.
-}
class Eq a => SILEq a where
  sileq :: a -> a
  default sileq :: (Generic a, All2 SILEq (Code a)) => a -> a
  sileq = gsileq

gsileq :: (Generic a, All2 SILEq (Code a)) => a -> a
gsileq = to . hcmap (Proxy :: Proxy SILEq) (mapII sileq) . from

{- | Equality without SourceInfo.
 If the type does not contain SourceInfo then: (==) = (=:=).
-}
(=:=) :: SILEq a => a -> a -> Bool
(=:=) a b = sileq a == sileq b

instance SILEq a => SILEq [a] where
  sileq = fmap sileq

instance SILEq Int where
  sileq = id

instance SILEq Text where
  sileq = id

instance (Ord k, SILEq k, SILEq v) => SILEq (M.Map k v) where
  sileq = M.fromList . fmap (bimap sileq sileq) . M.toList

instance (Ord a, SILEq a) => SILEq (S.Set a) where
  sileq = S.fromList . fmap sileq . S.toList
