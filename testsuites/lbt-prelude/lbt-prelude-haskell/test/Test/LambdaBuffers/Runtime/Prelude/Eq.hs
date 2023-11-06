module Test.LambdaBuffers.Runtime.Prelude.Eq (tests) where

import Hedgehog qualified as H
import Test.LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

tests :: TestTree
tests =
  testGroup
    "lbf-prelude.Prelude.Eq class derivation tests"
    [hedgehogTests]

hedgehogTests :: TestTree
hedgehogTests =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Property tests"
      [ eqTest "Foo.A" Correct.genA
      , eqTest
          "Foo.B"
          Correct.genB
      , eqTest
          "Foo.C"
          Correct.genC
      , eqTest
          "Foo.D"
          Correct.genD
      , eqTest
          "Foo.FInt"
          Correct.genFInt
      , eqTest
          "Foo.GInt"
          Correct.genGInt
      , eqTest
          "Days.Day"
          Correct.genDay
      , eqTest
          "Days.WorkDay"
          Correct.genWorkDay
      , eqTest
          "Days.WeekDay"
          Correct.genWeekDay
      ]

eqTest :: forall {a}. (Show a, Eq a) => TestName -> H.Gen a -> TestTree
eqTest title gen =
  testProperty
    (title <> ": x == x")
    ( H.property $ do
        x <- H.forAll gen
        x H.=== x
    )
