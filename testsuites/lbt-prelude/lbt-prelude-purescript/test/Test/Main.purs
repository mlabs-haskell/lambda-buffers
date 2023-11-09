module Test.Main
  ( main
  ) where

import Prelude
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.LambdaBuffers.Prelude.Golden.Json as GoldenJson
import Test.Spec (describe)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

-- TODO(bladyjoker): Ugh, clean up this remarkably complicated mess.
main :: Effect Unit
main = do
  goldenJson <- GoldenJson.tests
  either (fail <<< show) (launchAff_)
    ( map (const unit)
        <$> runSpecT defaultConfig [ consoleReporter ] do
            describe "LambdaBuffers Prelude runtime tests" do
              goldenJson
    )
