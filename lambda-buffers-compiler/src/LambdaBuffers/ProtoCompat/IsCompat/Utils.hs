module LambdaBuffers.ProtoCompat.IsCompat.Utils (parseAndIndex, parseAndIndex') where

import Data.Foldable (foldlM)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import LambdaBuffers.ProtoCompat.IsCompat.FromProto (FromProto, IsCompat (fromProto))

parseAndIndex :: forall {t :: Type -> Type} {proto} {a} {k}. (Foldable t, IsCompat proto a, Ord k) => (a -> k) -> t proto -> FromProto (Map k a, Map k [proto])
parseAndIndex key =
  foldlM
    ( \(indexed, multiples) px -> do
        x <- fromProto px
        let k = key x
        if Map.member k indexed
          then return (indexed, Map.insertWith (++) k [px] multiples)
          else return (Map.insert k x indexed, multiples)
    )
    (mempty, mempty)

parseAndIndex' :: forall {t :: Type -> Type} {proto} {a} {k}. (Foldable t, IsCompat proto a, Ord k) => (a -> k) -> t proto -> FromProto (OMap k a, Map k [proto])
parseAndIndex' key =
  foldlM
    ( \(indexed, multiples) px -> do
        x <- fromProto px
        let k = key x
        if OMap.member k indexed
          then return (indexed, Map.insertWith (++) k [px] multiples)
          else return (OMap.alter (const (Just x)) k indexed, multiples)
    )
    (OMap.empty, mempty)
