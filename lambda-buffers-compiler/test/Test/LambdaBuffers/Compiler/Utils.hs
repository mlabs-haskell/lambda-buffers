module Test.LambdaBuffers.Compiler.Utils (distribute, partition, indexBy, pick) where

import Control.Monad (foldM)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set (Set)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H

-- | Distributes values (first argument) over the keys (second) randomly.
distribute :: Foldable t => Ord k => t v -> Set k -> H.Gen (Map k (NonEmpty v))
distribute vals keys = do
  (leftover, distributed) <- distributeSingle vals keys
  if null leftover
    then return distributed
    else do
      distributed' <- distribute leftover keys
      return $ Map.unionWith (<>) distributed distributed'

distributeSingle :: Foldable t => Ord k => t v -> Set k -> H.Gen ([v], Map k (NonEmpty v))
distributeSingle vals =
  foldM
    ( \(vals', dist) key ->
        case vals' of
          [] -> return (vals', dist)
          (v : vals'') -> do
            (chosenVals, leftoverVals) <- partition vals''
            return (leftoverVals, Map.insert key (v :| chosenVals) dist)
    )
    (toList vals, mempty)

-- | Partition a list randomly.
partition :: forall {a}. [a] -> H.Gen ([a], [a])
partition xs = go xs []
  where
    go :: [a] -> [a] -> H.Gen ([a], [a])
    go [] outs = return (outs, [])
    go (i : ins) outs = do
      b <- H.bool
      if b
        then go ins (i : outs)
        else return (outs, i : ins)

-- | Pick an element randomly.
pick :: forall {a}. NonEmpty a -> H.Gen (a, [a])
pick (x :| xs) = go x xs []
  where
    go :: t -> [t] -> [t] -> H.Gen (t, [t])
    go champion [] losers = return (champion, losers)
    go champion (challenger : challengers) losers = do
      championWins <- H.bool
      if championWins
        then go champion challengers (challenger : losers)
        else go challenger challengers (champion : losers)

_indexBy :: Ord k => (a -> k) -> NonEmpty a -> NEMap k a
_indexBy keyF (x :| xs) = foldl (\t x' -> NEMap.insert (keyF x') x' t) (NEMap.singleton (keyF x) x) xs

-- | Index a list given a key function.
indexBy :: Foldable t => Ord k => (a -> k) -> t a -> Map k a
indexBy keyF = foldl (\t x -> Map.insert (keyF x) x t) mempty
