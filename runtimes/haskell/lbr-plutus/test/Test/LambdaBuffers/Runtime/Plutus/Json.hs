module Test.LambdaBuffers.Runtime.Plutus.Json (test) where

import Hedgehog qualified as H
import LambdaBuffers.Runtime.Plutus ()
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import Test.LambdaBuffers.Plutus.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "lbf-prelude.Plutus.Json type class tests"
      [ testGroup "Instance" instanceFromTo
      ]

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

instanceFromTo :: [TestTree]
instanceFromTo =
  [ fromToTest
      "Plutus.V1.Value"
      Correct.genValue
  , fromToTest
      "Plutus.V1.CurrencySymbol"
      Correct.genCurrencySymbol
  , fromToTest
      "Plutus.V1.AssetClass"
      Correct.genAssetClass
  , fromToTest
      "Plutus.V1.TokenName"
      Correct.genTokenName
  , fromToTest
      "Plutus.V1.Data"
      Correct.genData
  , fromToTest
      "Plutus.V1.PubKeyHash"
      Correct.genPubKeyHash
  , fromToTest
      "Plutus.V1.ScriptHash"
      Correct.genScriptHash
  , fromToTest
      "Plutus.V1.RedeemerHash"
      Correct.genRedeemerHash
  , fromToTest
      "Plutus.V1.DatumHash"
      Correct.genDatumHash
  , fromToTest
      "Plutus.V1.Interval PlutusV1.POSIXTime"
      Correct.genInterval
  , fromToTest
      "Plutus.V1.Extended PlutusV1.POSIXTime"
      Correct.genExtended
  , fromToTest
      "Plutus.V1.UpperBound PlutusV1.POSIXTime"
      Correct.genUpperBound
  , fromToTest
      "Plutus.V1.LowerBound PlutusV1.POSIXTime"
      Correct.genLowerBound
  , fromToTest
      "Plutus.V1.POSIXTime"
      Correct.genPosixTime
  , fromToTest
      "Plutus.V1.Closure"
      Correct.genClosure
  , fromToTest
      "Plutus.V1.Address"
      Correct.genAddress
  , fromToTest
      "Plutus.V1.Credential"
      Correct.genCredential
  , fromToTest
      "Plutus.V1.StakingCredential"
      Correct.genStakingCredential
  , fromToTest
      "Plutus.V1.Datum"
      Correct.genDatum
  , fromToTest
      "Plutus.V1.Redeemer"
      Correct.genRedeemer
  , fromToTest
      "Plutus.V1.TxId"
      Correct.genTxId
  , fromToTest
      "Plutus.V1.TxOutRef"
      Correct.genTxOutRef
  , fromToTest
      "Plutus.V2.TxOut"
      Correct.genTxOut
  , fromToTest
      "Plutus.V2.OutputDatum"
      Correct.genOutputDatum
  , fromToTest
      "Plutus.V2.TxInInfo"
      Correct.genTxInInfo
  ]
