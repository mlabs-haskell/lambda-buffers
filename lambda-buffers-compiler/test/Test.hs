{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.KindCheck qualified as KC
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "All Tests" [KC.test]
