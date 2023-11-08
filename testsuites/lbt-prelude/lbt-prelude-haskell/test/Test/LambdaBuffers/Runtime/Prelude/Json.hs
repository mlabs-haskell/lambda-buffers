module Test.LambdaBuffers.Runtime.Prelude.Json (tests) where

import Hedgehog qualified as H
import LambdaBuffers.Prelude.Json.Golden qualified as Golden
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import Paths_lbt_prelude_golden_data qualified as Paths
import Test.LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

tests :: IO TestTree
tests = do
  golden <- goldenTests
  return $
    testGroup
      "lbf-prelude.Prelude.Json class derivation tests"
      [golden, hedgehogTests]

hedgehogTests :: TestTree
hedgehogTests =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Property tests"
      [ toFromTest
          "Foo.A"
          Correct.genA
      , toFromTest
          "Foo.B"
          Correct.genB
      , toFromTest
          "Foo.C"
          Correct.genC
      , toFromTest
          "Foo.D"
          Correct.genD
      , toFromTest
          "Foo.FInt"
          Correct.genFInt
      , toFromTest
          "Foo.GInt"
          Correct.genGInt
      , toFromTest
          "Days.Day"
          Correct.genDay
      , toFromTest
          "Days.WorkDay"
          Correct.genWorkDay
      , toFromTest
          "Days.WeekDay"
          Correct.genWeekDay
      ]

goldenTests :: IO TestTree
goldenTests =
  testGroup "Golden tests"
    <$> sequenceA
      [ daysFromToGoldenTests
      , fooFromToGoldenTests
      , preludeFromToGoldenTests
      ]

toFromTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
toFromTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

fromToGoldenTest :: forall {a}. Json a => TestName -> [a] -> IO TestTree
fromToGoldenTest title goldens = do
  goldenDir <- Paths.getDataFileName "data/"
  Golden.fromToGoldenTest goldenDir title goldens

fooFromToGoldenTests :: IO TestTree
fooFromToGoldenTests =
  testGroup "Foo"
    <$> sequenceA
      [ fromToGoldenTest
          "Foo.A"
          Golden.aGoldens
      , fromToGoldenTest
          "Foo.B"
          Golden.bGoldens
      , fromToGoldenTest
          "Foo.C"
          Golden.cGoldens
      , fromToGoldenTest
          "Foo.D"
          Golden.dGoldens
      , fromToGoldenTest
          "Foo.FInt"
          Golden.fIntGoldens
      , fromToGoldenTest
          "Foo.GInt"
          Golden.gIntGoldens
      ]

daysFromToGoldenTests :: IO TestTree
daysFromToGoldenTests =
  testGroup "Days"
    <$> sequenceA
      [ fromToGoldenTest "Days.Day" Golden.dayGoldens
      , fromToGoldenTest "Days.WorkDay" Golden.workDayGoldens
      , fromToGoldenTest "Days.FreeDay" Golden.freeDayGoldens
      ]

preludeFromToGoldenTests :: IO TestTree
preludeFromToGoldenTests =
  testGroup "Prelude"
    <$> sequenceA
      [ fromToGoldenTest "Prelude.Bool" Golden.boolGoldens
      , fromToGoldenTest "Prelude.Char" Golden.charGoldens
      , fromToGoldenTest "Prelude.Integer" Golden.integerGoldens
      , fromToGoldenTest "Prelude.Text" Golden.textGoldens
      , fromToGoldenTest "Prelude.Bytes" Golden.bytesGoldens
      , fromToGoldenTest "Prelude.Maybe" Golden.maybeGoldens
      , fromToGoldenTest "Prelude.Either" Golden.eitherGoldens
      , fromToGoldenTest "Prelude.List" Golden.listGoldens
      , fromToGoldenTest "Prelude.Set" Golden.setGoldens
      , fromToGoldenTest "Prelude.Map" Golden.mapGoldens
      ]
