module Test.Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.LambdaBuffers.Runtime.Plutus.PlutusData as PlutusData
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "LambdaBuffers Plutus runtime tests" do
          PlutusData.tests
