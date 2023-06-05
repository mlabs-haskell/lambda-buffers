module Test.LambdaBuffers.Runtime.Json.Plutus (test) where

import Hedgehog qualified as H
import LambdaBuffers.Runtime.Json.Plutus (Json, fromJsonBytes, toJsonBytes)
import Test.LambdaBuffers.Runtime.Json.Plutus.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Compiler API tests"
      [ valueFromTo
      , currencySymbolFromTo
      , assetClassFromTo
      , tokenNameFromTo
      ]

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    title
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

valueFromTo :: TestTree
valueFromTo =
  fromToTest
    "Plutus.V1.Value: (fromJson . toJson) x == x"
    Correct.genValue

currencySymbolFromTo :: TestTree
currencySymbolFromTo =
  fromToTest
    "Plutus.V1.CurrencySymbol: (fromJson . toJson) x == x"
    Correct.genCurrencySymbol

assetClassFromTo :: TestTree
assetClassFromTo =
  fromToTest
    "Plutus.V1.AssetClass: (fromJson . toJson) x == x"
    Correct.genAssetClass

tokenNameFromTo :: TestTree
tokenNameFromTo =
  fromToTest
    "Plutus.V1.TokenName: (fromJson . toJson) x == x"
    Correct.genTokenName
