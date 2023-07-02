module Test.LambdaBuffers.Runtime.Plutus
  ( tests
  ) where

import Prelude
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.ToData (class ToData, toData)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.LambdaBuffers.Plutus.Generators.Correct as Correct
import Test.QuickCheck (quickCheckGen, (===)) as Q
import Test.QuickCheck.Gen (Gen) as Q
import Test.Spec (Spec, describe, it)

tests :: Spec Unit
tests = do
  describe "lbf-plutus.Plutus.PlutusData class" do
    describe "Instance" do
      fromToTest "Plutus.V1.Integer" Correct.genInteger
      fromToTest "Plutus.V1..Bool" Correct.genBool
      fromToTest "Plutus.V1..Bytes" (Correct.genPlutusBytes 5)
      fromToTest "Plutus.V1.CurrencySymbol" Correct.genCurrencySymbol
      fromToTest "Plutus.V1.TokenName" Correct.genTokenName
      fromToTest "Plutus.V1.Value" Correct.genValue
      fromToTest "Plutus.V1.PlutusData" Correct.genData
      fromToTest "Plutus.V1.Datum" Correct.genDatum
      fromToTest "Plutus.V1.Redeemer" Correct.genRedeemer
      fromToTest "Plutus.V1.PubKeyHash" Correct.genPubKeyHash
      fromToTest "Plutus.V1.DatumHash" Correct.genDatumHash
      fromToTest "Plutus.V1.RedeemerHash" Correct.genRedeemerHash
      fromToTest "Plutus.V1.Extended" Correct.genExtended
      fromToTest "Plutus.V1.Closure" Correct.genClosure
      fromToTest "Plutus.V1.LowerBound" Correct.genLowerBound
      fromToTest "Plutus.V1.UpperBound" Correct.genUpperBound
      fromToTest "Plutus.V1.Interval" Correct.genInterval
      fromToTest "Plutus.V1.Credential" Correct.genCredential
      fromToTest "Plutus.V1.StakingCredential" Correct.genStakingCredential
      fromToTest "Plutus.V1.Address" Correct.genAddress
      fromToTest "Plutus.V1.TxId" Correct.genTxId
      fromToTest "Plutus.V1.TxOutRef" Correct.genTxOutRef
      fromToTest "Plutus.V2.OutputDatum" Correct.genOutputDatum
      fromToTest "Plutus.V2.TxOut" Correct.genTxOut
      fromToTest "Plutus.V2.TxInInfo" Correct.genTxInInfo

fromToTest :: forall a. ToData a => FromData a => Show a => Eq a => String -> Q.Gen a -> Spec Unit
fromToTest title gen =
  it ("forall (x: " <> title <> "): (fromData . toData) x == x") do
    liftEffect
      $ Q.quickCheckGen do
          x <- gen
          pure $ (toData >>> fromData) x Q.=== Just x
