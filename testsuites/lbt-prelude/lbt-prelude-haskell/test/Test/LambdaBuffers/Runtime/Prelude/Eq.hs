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
      [ aEq
      , bEq
      , cEq
      , dEq
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

aEq :: TestTree
aEq =
  eqTest
    "Foo.A"
    Correct.genA

bEq :: TestTree
bEq =
  eqTest
    "Foo.B"
    Correct.genB

cEq :: TestTree
cEq =
  eqTest
    "Foo.C"
    Correct.genC

dEq :: TestTree
dEq =
  eqTest
    "Foo.D"
    Correct.genD

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
