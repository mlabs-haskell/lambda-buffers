module Test.Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.LambdaBuffers.Runtime.Prelude.Json as PreludeJson
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "LambdaBuffers Prelude runtime tests" do
          PreludeJson.tests
