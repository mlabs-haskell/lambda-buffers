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
      "`lbf-plutus` package tests for lbf-prelude.Plutus.Json type class implementations"
      [ valueFromTo
      , currencySymbolFromTo
      , assetClassFromTo
      , tokenNameFromTo
      , dataFromTo
      , pubKeyHashFromTo
      , scriptHashFromTo
      , redeemerHashFromTo
      , datumHashFromTo
      , intervalFromTo
      , extendedFromTo
      , upperBoundFromTo
      , lowerBoundFromTo
      , posixTimeFromTo
      , closureFromTo
      , addressFromTo
      , credentialFromTo
      , stakingCredentialFromTo
      , datumFromTo
      , redeemerFromTo
      , txIdFromTo
      , txOutFromTo
      , txOutRefFromTo
      , outputDatumFromTo
      ]

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

valueFromTo :: TestTree
valueFromTo =
  fromToTest
    "Plutus.V1.Value"
    Correct.genValue

currencySymbolFromTo :: TestTree
currencySymbolFromTo =
  fromToTest
    "Plutus.V1.CurrencySymbol"
    Correct.genCurrencySymbol

assetClassFromTo :: TestTree
assetClassFromTo =
  fromToTest
    "Plutus.V1.AssetClass"
    Correct.genAssetClass

tokenNameFromTo :: TestTree
tokenNameFromTo =
  fromToTest
    "Plutus.V1.TokenName"
    Correct.genTokenName

dataFromTo :: TestTree
dataFromTo =
  fromToTest
    "Plutus.V1.Data"
    Correct.genData

pubKeyHashFromTo :: TestTree
pubKeyHashFromTo =
  fromToTest
    "Plutus.V1.PubKeyHash"
    Correct.genPubKeyHash

scriptHashFromTo :: TestTree
scriptHashFromTo =
  fromToTest
    "Plutus.V1.ScriptHash"
    Correct.genScriptHash

redeemerHashFromTo :: TestTree
redeemerHashFromTo =
  fromToTest
    "Plutus.V1.RedeemerHash"
    Correct.genRedeemerHash

datumHashFromTo :: TestTree
datumHashFromTo =
  fromToTest
    "Plutus.V1.DatumHash"
    Correct.genDatumHash

intervalFromTo :: TestTree
intervalFromTo =
  fromToTest
    "Plutus.V1.Interval PlutusV1.POSIXTime"
    Correct.genInterval

extendedFromTo :: TestTree
extendedFromTo =
  fromToTest
    "Plutus.V1.Extended PlutusV1.POSIXTime"
    Correct.genExtended

upperBoundFromTo :: TestTree
upperBoundFromTo =
  fromToTest
    "Plutus.V1.UpperBound PlutusV1.POSIXTime"
    Correct.genUpperBound

lowerBoundFromTo :: TestTree
lowerBoundFromTo =
  fromToTest
    "Plutus.V1.LowerBound PlutusV1.POSIXTime"
    Correct.genLowerBound

posixTimeFromTo :: TestTree
posixTimeFromTo =
  fromToTest
    "Plutus.V1.POSIXTime"
    Correct.genPosixTime

closureFromTo :: TestTree
closureFromTo =
  fromToTest
    "Plutus.V1.Closure"
    Correct.genClosure

addressFromTo :: TestTree
addressFromTo =
  fromToTest
    "Plutus.V1.Address"
    Correct.genAddress

credentialFromTo :: TestTree
credentialFromTo =
  fromToTest
    "Plutus.V1.Credential"
    Correct.genCredential

stakingCredentialFromTo :: TestTree
stakingCredentialFromTo =
  fromToTest
    "Plutus.V1.StakingCredential"
    Correct.genStakingCredential

datumFromTo :: TestTree
datumFromTo =
  fromToTest
    "Plutus.V1.Datum"
    Correct.genDatum

redeemerFromTo :: TestTree
redeemerFromTo =
  fromToTest
    "Plutus.V1.Redeemer"
    Correct.genRedeemer

txIdFromTo :: TestTree
txIdFromTo =
  fromToTest
    "Plutus.V1.TxId"
    Correct.genTxId

txOutRefFromTo :: TestTree
txOutRefFromTo =
  fromToTest
    "Plutus.V1.TxOutRef"
    Correct.genTxOutRef

txOutFromTo :: TestTree
txOutFromTo =
  fromToTest
    "Plutus.V2.TxOut"
    Correct.genTxOut

outputDatumFromTo :: TestTree
outputDatumFromTo =
  fromToTest
    "Plutus.V2.OutputDatum"
    Correct.genOutputDatum
