{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.ProtoCompat.InfoLess (
  InfoLess,
  withInfoLess,
  withInfoLessF,
  mkInfoLess,
  InfoLessC (infoLessId),
) where

import Data.Map (Map)
import Data.Map.Ordered (OMap)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Generics.SOP (All2, Generic (Code, from, to), Proxy (Proxy), hcmap, mapII)

-- | InfoLess newtype. Constructor is not exported to not allow the construction of types with the Info. InfoLess a can only be constructed via its class instance and deconstructed using the exported function.
newtype InfoLess a = InfoLess {unsafeInfoLess :: a}
  deriving stock (Show, Eq, Ord)
  deriving stock (Functor, Traversable, Foldable)

{- | SourceInfo Less ID.
 A TypeClass that provides id for types with SourceInfo - where SI is defaulted - therefore ignored. Can only be derived.
-}
class Eq a => InfoLessC a where
  infoLessId :: a -> a
  default infoLessId :: (Generic a, All2 InfoLessC (Code a)) => a -> a
  infoLessId = ginfoLessId

-- | Make InfoLess Datatype.
mkInfoLess :: InfoLessC a => a -> InfoLess a
mkInfoLess = InfoLess . infoLessId

ginfoLessId :: (Generic a, All2 InfoLessC (Code a)) => a -> a
ginfoLessId = to . hcmap (Proxy :: Proxy InfoLessC) (mapII infoLessId) . from

-- | Work with Info Less.
withInfoLess :: InfoLessC a => InfoLess a -> (a -> b) -> b
withInfoLess (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

-- | Work with Info Less - functor way.
withInfoLessF :: forall f {a} {b}. InfoLessC a => InfoLess a -> (a -> f b) -> f b
withInfoLessF (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

instance InfoLessC a => InfoLessC [a] where
  infoLessId = fmap infoLessId

instance InfoLessC Int where
  infoLessId = id

instance InfoLessC Text where
  infoLessId = id

instance (Ord k, InfoLessC v) => InfoLessC (Map k v) where
  infoLessId m = infoLessId <$> m

instance (Ord a, InfoLessC a) => InfoLessC (Set a) where
  infoLessId = S.fromList . fmap infoLessId . S.toList

instance (Ord k, InfoLessC v) => InfoLessC (OMap k v) where
  infoLessId om = infoLessId <$> om
