module Test.LambdaBuffers.Runtime.Plutus.PlutusData (tests) where

import Prelude
import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.LambdaBuffers.Runtime.Plutus.Generators.Correct as Correct
import Test.QuickCheck (quickCheckGen, (===)) as Q
import Test.QuickCheck.Gen (Gen) as Q
import Test.Spec (Spec, describe, it)

tests :: Spec Unit
tests = do
  describe "Plutus.V1.PlutusData type class tests" do
    describe "Derive" do
      describe "Foo" do
        fromToTest "Foo.A" Correct.genA
        fromToTest "Foo.B" Correct.genB
        fromToTest "Foo.C" Correct.genC
        fromToTest "Foo.D" Correct.genD
        fromToTest "Foo.FInt" Correct.genFInt
        fromToTest "Foo.GInt" Correct.genGInt
      describe "Days" do
        fromToTest "Days.Day" Correct.genDay
        fromToTest "Days.WorkDay" Correct.genWorkDay
        fromToTest "Days.FreeDay" Correct.genFreeDay
      describe "Prelude" do
        fromToTest "Prelude.Bool" Correct.genBool
        fromToTest "Prelude.Maybe" Correct.genMaybe
        fromToTest "Prelude.Either" Correct.genEither
        fromToTest "Prelude.List" Correct.genList

fromToTest :: forall a. ToData a => FromData a => Show a => Eq a => String -> Q.Gen a -> Spec Unit
fromToTest title gen =
  it ("forall (x: " <> title <> "): (fromData . toData) x == x") do
    liftEffect
      $ Q.quickCheckGen do
          x <- gen
          pure $ (toData >>> fromData) x Q.=== Just x
