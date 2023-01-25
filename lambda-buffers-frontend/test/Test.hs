module Main (main) where

import Test.LambdaBuffers.Frontend (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Frontend tests" [tests "resources"]
