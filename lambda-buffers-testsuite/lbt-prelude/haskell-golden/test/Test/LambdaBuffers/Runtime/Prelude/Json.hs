module Test.LambdaBuffers.Runtime.Prelude.Json (test) where

import Hedgehog qualified as H
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Lbr
import Test.LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "lbf-prelude.Prelude.Json class derivation tests"
      [ fooSumFromTo
      , fooProdFromTo
      , fooRecFromTo
      , fooComplicatedFromTo
      , dayFromTo
      , workDayFromTo
      , weekDayFromTo
      ]

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

fooSumFromTo :: TestTree
fooSumFromTo =
  fromToTest
    "Foo.Bar.FooSum"
    (Correct.genFooSum Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooProdFromTo :: TestTree
fooProdFromTo =
  fromToTest
    "Foo.Bar.FooProd"
    (Correct.genFooProd Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooRecFromTo :: TestTree
fooRecFromTo =
  fromToTest
    "Foo.Bar.FooRec"
    (Correct.genFooRec Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooComplicatedFromTo :: TestTree
fooComplicatedFromTo =
  fromToTest
    "Foo.Bar.FooComplicated"
    (Correct.genFooComplicated Lbr.genInteger Lbr.genBool Lbr.genBytes)

dayFromTo :: TestTree
dayFromTo =
  fromToTest
    "Days.Day"
    Correct.genDay

workDayFromTo :: TestTree
workDayFromTo =
  fromToTest
    "Days.WorkDay"
    Correct.genWorkDay

weekDayFromTo :: TestTree
weekDayFromTo =
  fromToTest
    "Days.WeekDay"
    Correct.genWeekDay
