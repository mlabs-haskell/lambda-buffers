module Test.LambdaBuffers.Compiler.Gen.Utils (distribute, partition, indexBy, vecOf, nevecOf, setOf, nesetOf, pick) where

import Control.Monad (foldM)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Test.QuickCheck.Gen qualified as QC

-- | Distributes values (first argument) over the keys (second) randomly.
distribute :: Foldable t => Ord k => t v -> Set k -> QC.Gen (Map k (NonEmpty v))
distribute vals keys = do
  (leftover, distributed) <- distributeSingle vals keys
  if null leftover
    then return distributed
    else do
      distributed' <- distribute leftover keys
      return $ Map.unionWith (<>) distributed distributed'

distributeSingle :: Foldable t => Ord k => t v -> Set k -> QC.Gen ([v], Map k (NonEmpty v))
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
partition :: forall {a}. [a] -> QC.Gen ([a], [a])
partition xs = go xs []
  where
    go :: [a] -> [a] -> QC.Gen ([a], [a])
    go [] outs = return (outs, [])
    go (i : ins) outs = do
      b <- QC.chooseAny
      if b
        then go ins (i : outs)
        else return (outs, i : ins)

-- | Pick an element randomly.
pick :: forall {a}. NonEmpty a -> QC.Gen (a, [a])
pick (x :| xs) = go x xs []
  where
    go :: t -> [t] -> [t] -> QC.Gen (t, [t])
    go champion [] losers = return (champion, losers)
    go champion (challenger : challengers) losers = do
      championWins <- QC.chooseAny
      if championWins
        then go champion challengers (challenger : losers)
        else go challenger challengers (champion : losers)

_indexBy :: Ord k => (a -> k) -> NonEmpty a -> NEMap k a
_indexBy keyF (x :| xs) = foldl (\t x' -> NEMap.insert (keyF x') x' t) (NEMap.singleton (keyF x) x) xs

-- | Index a list given a key function.
indexBy :: Foldable t => Ord k => (a -> k) -> t a -> Map k a
indexBy keyF = foldl (\t x -> Map.insert (keyF x) x t) mempty

vecOf :: forall {a}. QC.Gen a -> Int -> QC.Gen [a]
vecOf = flip QC.vectorOf

nevecOf :: forall {a}. QC.Gen a -> Int -> QC.Gen (NonEmpty a)
nevecOf g n =
  g >>= \x -> do
    xs <- QC.vectorOf (n - 1) g
    return $ x :| xs

nesetOf :: forall {a}. Ord a => QC.Gen a -> Int -> QC.Gen (NESet a)
nesetOf g n = NESet.fromList <$> nevecOf g n

setOf :: forall {a}. Ord a => QC.Gen a -> Int -> QC.Gen (Set a)
setOf g n = Set.fromList <$> vecOf g n
