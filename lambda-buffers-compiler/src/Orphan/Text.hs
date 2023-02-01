module Orphan.Text where

import Data.Text
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof, sized)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
