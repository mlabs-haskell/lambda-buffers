module Test.LambdaBuffers.Runtime.Prelude.Json
  ( tests
  ) where

import Prelude
import Data.Either (Either(Right), hush)
import Data.Maybe (Maybe(..))
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoder (encodeUtf8)
import Effect.Class (liftEffect)
import LambdaBuffers.Runtime.Prelude (class Json, toJsonString)
import LambdaBuffers.Runtime.Prelude.Bytes as Bytes
import LambdaBuffers.Runtime.Prelude.Generators.Correct as Correct
import LambdaBuffers.Runtime.Prelude.Json (fromJsonBytes, toJsonBytes)
import Test.QuickCheck (quickCheckGen, (===)) as Q
import Test.QuickCheck.Gen (Gen) as Q
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  describe "lbf-prelude.Prelude.Json class" do
    describe "Instance" do
      fromToTest "Prelude.Integer" Correct.genInteger
      fromToTest "Prelude.Bool" Correct.genBool
      fromToTest "Prelude.Char" Correct.genChar
      fromToTest "Prelude.Bytes" Correct.genBytes
      fromToTest "Prelude.Text" Correct.genText
      fromToTest "Prelude.Maybe" Correct.genMaybe
      fromToTest "Prelude.Either" Correct.genEither
      fromToTest "Prelude.List" Correct.genList
      fromToTest "Prelude.Set" Correct.genSet
      fromToTest "Prelude.Map" Correct.genMap

fromToTest :: forall a. Json a => Show a => Eq a => String -> Q.Gen a -> Spec Unit
fromToTest title gen =
  it ("forall (x: " <> title <> "): (fromJson . toJson) x == x") do
    liftEffect
      $ Q.quickCheckGen do
          x <- gen
          pure $ (toJsonBytes >>> fromJsonBytes) x Q.=== Right x
