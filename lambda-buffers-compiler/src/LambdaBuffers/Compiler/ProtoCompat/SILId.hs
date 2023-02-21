{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Compiler.ProtoCompat.SILId (
  (=:=),
  SILId (silId),
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Generics.SOP (All2, Generic (Code, from, to), Proxy (..), hcmap, mapII)

{- | SourceInfo Less ID.
 A TypeClass that provides id for types with SourceInfo - where SI is defaulted - therefore ignored.
-}
class Eq a => SILId a where
  silId :: a -> a
  default silId :: (Generic a, All2 SILId (Code a)) => a -> a
  silId = gsilId

gsilId :: (Generic a, All2 SILId (Code a)) => a -> a
gsilId = to . hcmap (Proxy :: Proxy SILId) (mapII silId) . from

{- | Equality without SourceInfo.
 If the type does not contain SourceInfo then: (==) = (=:=).
-}
(=:=) :: SILId a => a -> a -> Bool
(=:=) a b = silId a == silId b

instance SILId a => SILId [a] where
  silId = fmap silId

instance SILId Int where
  silId = id

instance SILId Text where
  silId = id

instance (Ord k, SILId k, SILId v) => SILId (M.Map k v) where
  silId = M.fromList . fmap (bimap silId silId) . M.toList

instance (Ord a, SILId a) => SILId (S.Set a) where
  silId = S.fromList . fmap silId . S.toList
