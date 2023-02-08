{-# OPTIONS_GHC -Wno-orphans #-}

module Orphan.Text () where

import Data.Text (Text, pack)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, sized)

instance Arbitrary Text where
  arbitrary = sized f
    where
      f :: (Ord a, Num a) => a -> Gen Text
      f n
        | n <= 0 = pure $ pack []
        | otherwise = do
            c <- arbitrary
            cs <- f (n - 1)
            pure $ pack [c] <> cs
