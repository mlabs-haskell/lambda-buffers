module Test.LambdaBuffers.Runtime.Json.Prelude (test) where

import Hedgehog qualified as H
import LambdaBuffers.Runtime.Json (Json, fromJsonBytes, toJsonBytes)
import Test.LambdaBuffers.Runtime.Json.Prelude.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "lbf-prelude.Prelude.Json class instance implementation tests"
      [ integerFromTo
      , boolFromTo
      , charFromTo
      , bytesFromTo
      , textFromTo
      , maybeFromTo
      , eitherFromTo
      , listFromTo
      , setFromTo
      , mapFromTo
      , complicatedFromTo
      ]

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

integerFromTo :: TestTree
integerFromTo =
  fromToTest
    "Prelude.Integer"
    Correct.genInteger

boolFromTo :: TestTree
boolFromTo =
  fromToTest
    "Prelude.Bool"
    Correct.genBool

charFromTo :: TestTree
charFromTo =
  fromToTest
    "Prelude.Char"
    Correct.genChar

bytesFromTo :: TestTree
bytesFromTo =
  fromToTest
    "Prelude.Bytes"
    Correct.genBytes

textFromTo :: TestTree
textFromTo =
  fromToTest
    "Prelude.Text"
    Correct.genText

maybeFromTo :: TestTree
maybeFromTo =
  fromToTest
    "Prelude.Maybe"
    (Correct.genMaybe Correct.genInteger)

eitherFromTo :: TestTree
eitherFromTo =
  fromToTest
    "Prelude.Either"
    (Correct.genEither Correct.genBool Correct.genInteger)

listFromTo :: TestTree
listFromTo =
  fromToTest
    "Prelude.List"
    (Correct.genList Correct.genInteger)

setFromTo :: TestTree
setFromTo =
  fromToTest
    "Prelude.Set"
    (Correct.genSet Correct.genInteger)

mapFromTo :: TestTree
mapFromTo =
  fromToTest
    "Prelude.Map"
    (Correct.genMap Correct.genText Correct.genInteger)

complicatedFromTo :: TestTree
complicatedFromTo =
  fromToTest
    "Complicated type"
    Correct.genComplicated
