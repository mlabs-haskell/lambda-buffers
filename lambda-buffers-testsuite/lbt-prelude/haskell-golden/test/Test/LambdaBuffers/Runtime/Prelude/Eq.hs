module Test.LambdaBuffers.Runtime.Prelude.Eq (tests) where

import Hedgehog qualified as H
import LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Lbr
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
      [ fooSumEq
      , fooProdEq
      , fooRecEq
      , fooComplicatedEq
      , dayEq
      , workDayEq
      , weekDayEq
      ]

eqTest :: forall {a}. (Show a, Eq a) => TestName -> H.Gen a -> TestTree
eqTest title gen =
  testProperty
    (title <> ": x == x")
    ( H.property $ do
        x <- H.forAll gen
        x H.=== x
    )

fooSumEq :: TestTree
fooSumEq =
  eqTest
    "Foo.Bar.FooSum"
    (Correct.genFooSum Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooProdEq :: TestTree
fooProdEq =
  eqTest
    "Foo.Bar.FooProd"
    (Correct.genFooProd Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooRecEq :: TestTree
fooRecEq =
  eqTest
    "Foo.Bar.FooRec"
    (Correct.genFooRec Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooComplicatedEq :: TestTree
fooComplicatedEq =
  eqTest
    "Foo.Bar.FooComplicated"
    (Correct.genFooComplicated Lbr.genInteger Lbr.genBool Lbr.genBytes)

dayEq :: TestTree
dayEq =
  eqTest
    "Days.Day"
    Correct.genDay

workDayEq :: TestTree
workDayEq =
  eqTest
    "Days.WorkDay"
    Correct.genWorkDay

weekDayEq :: TestTree
weekDayEq =
  eqTest
    "Days.WeekDay"
    Correct.genWeekDay
