module Main (main) where

import Test.LambdaBuffers.Codegen (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Codegen tests" [tests]
