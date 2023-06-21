module Test.LambdaBuffers.Runtime.Prelude.Json (tests) where

import Hedgehog qualified as H
import LambdaBuffers.Prelude.Json.Golden qualified as Golden
import LambdaBuffers.Prelude.Json.Golden qualified as Map
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import Paths_lbt_prelude_golden_data_hs qualified as Paths
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
      [ aToFrom
      , bToFrom
      , cToFrom
      , dToFrom
      , dayToFrom
      , workDayToFrom
      , weekDayToFrom
      ]

goldenTests :: IO TestTree
goldenTests = do
  gts <-
    id
      `traverse` [ dayFromToGolden
                 , workDayFromToGolden
                 , weekDayFromToGolden
                 , aFromToGolden
                 , bFromToGolden
                 , cFromToGolden
                 , dFromToGolden
                 , boolFromToGolden
                 , integerFromToGolden
                 , bytesFromToGolden
                 , charFromToGolden
                 , textFromToGolden
                 , maybeFromToGolden
                 , eitherFromToGolden
                 , listFromToGolden
                 , setFromToGolden
                 , mapFromToGolden
                 ]

  return $
    testGroup
      "Golden tests"
      gts

toFromTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
toFromTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

aToFrom :: TestTree
aToFrom =
  toFromTest
    "Foo.A"
    Correct.genA

bToFrom :: TestTree
bToFrom =
  toFromTest
    "Foo.B"
    Correct.genB

cToFrom :: TestTree
cToFrom =
  toFromTest
    "Foo.C"
    Correct.genC

dToFrom :: TestTree
dToFrom =
  toFromTest
    "Foo.D"
    Correct.genD

dayToFrom :: TestTree
dayToFrom =
  toFromTest
    "Days.Day"
    Correct.genDay

workDayToFrom :: TestTree
workDayToFrom =
  toFromTest
    "Days.WorkDay"
    Correct.genWorkDay

weekDayToFrom :: TestTree
weekDayToFrom =
  toFromTest
    "Days.WeekDay"
    Correct.genWeekDay

fromToGoldenTest :: forall {a}. Json a => TestName -> [a] -> IO TestTree
fromToGoldenTest title goldens = do
  goldenDir <- Paths.getDataFileName "data/golden"
  Golden.fromToGoldenTest goldenDir title goldens

aFromToGolden :: IO TestTree
aFromToGolden =
  fromToGoldenTest
    "Foo.A"
    Golden.aGoldens

bFromToGolden :: IO TestTree
bFromToGolden =
  fromToGoldenTest
    "Foo.B"
    Golden.bGoldens

cFromToGolden :: IO TestTree
cFromToGolden =
  fromToGoldenTest
    "Foo.C"
    Golden.cGoldens

dFromToGolden :: IO TestTree
dFromToGolden =
  fromToGoldenTest
    "Foo.D"
    Golden.dGoldens

dayFromToGolden :: IO TestTree
dayFromToGolden = fromToGoldenTest "Days.Day" Golden.dayGoldens

workDayFromToGolden :: IO TestTree
workDayFromToGolden = fromToGoldenTest "Days.WorkDay" Golden.workDayGoldens

weekDayFromToGolden :: IO TestTree
weekDayFromToGolden = fromToGoldenTest "Days.FreeDay" Golden.freeDayGoldens

boolFromToGolden :: IO TestTree
boolFromToGolden = fromToGoldenTest "Prelude.Bool" Golden.boolGoldens

integerFromToGolden :: IO TestTree
integerFromToGolden =
  fromToGoldenTest
    "Prelude.Integer"
    Golden.integerGoldens

bytesFromToGolden :: IO TestTree
bytesFromToGolden = fromToGoldenTest "Prelude.Bytes" Golden.bytesGoldens

charFromToGolden :: IO TestTree
charFromToGolden = fromToGoldenTest "Prelude.Char" Golden.charGoldens

textFromToGolden :: IO TestTree
textFromToGolden = fromToGoldenTest "Prelude.Text" Golden.textGoldens

maybeFromToGolden :: IO TestTree
maybeFromToGolden = fromToGoldenTest "Prelude.Maybe" Golden.maybeGoldens

eitherFromToGolden :: IO TestTree
eitherFromToGolden = fromToGoldenTest "Prelude.Either" Golden.eitherGoldens

listFromToGolden :: IO TestTree
listFromToGolden = fromToGoldenTest "Prelude.List" Golden.listGoldens

setFromToGolden :: IO TestTree
setFromToGolden = fromToGoldenTest "Prelude.Set" Golden.setGoldens

mapFromToGolden :: IO TestTree
mapFromToGolden = fromToGoldenTest "Prelude.Map" Map.mapGoldens
