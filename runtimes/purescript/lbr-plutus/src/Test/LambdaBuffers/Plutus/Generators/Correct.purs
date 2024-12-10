module Test.LambdaBuffers.Plutus.Generators.Correct
  ( genAddress
  , genBool
  , genClosure
  , genCredential
  , genCurrencySymbol
  , genData
  , genDatum
  , genDatumHash
  , genExtended
  , genInteger
  , genInterval
  , genLowerBound
  , genMap
  , genOutputDatum
  , genPlutusBytes
  , genPosixTime
  , genPubKeyHash
  , genRedeemer
  , genScriptHash
  , genStakingCredential
  , genTokenName
  , genTxId
  , genTxInInfo
  , genTxOut
  , genTxOutRef
  , genUpperBound
  , genValue
  ) where

import Prelude
import Contract.Prelude (mconcat)
import Control.Alternative ((<|>))
import Contract.Prim.ByteArray (ByteArray(ByteArray))
import Cardano.Plutus.Types.Address (Address(Address)) as Types
import Cardano.Plutus.Types.CurrencySymbol (CurrencySymbol, adaSymbol, mkCurrencySymbol) as Types
import Cardano.Plutus.Types.Map (Map(Map)) as Map
import Cardano.Plutus.Types.TokenName (TokenName, adaToken, mkTokenName) as Types
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential, ScriptCredential)) as Types
import Cardano.Plutus.Types.Value (Value) as Types
import Cardano.Plutus.Types.Value (singleton) as Value
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash)) as Types
import Cardano.Types.ScriptHash (ScriptHash) as Types
import Cardano.Types.PlutusData (PlutusData(Integer, Map, Constr, List, Bytes)) as PlutusData
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput)) as Types
import Cardano.Types.DataHash (DataHash) as Types
import Ctl.Internal.Types.Interval as Time
import Cardano.Types.TransactionHash (TransactionHash) as Types
import Cardano.Plutus.Types.OutputDatum (OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)) as Types
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Plutus.Types.TransactionOutput (TransactionOutput(TransactionOutput)) as Types
import Cardano.Plutus.Types.StakingCredential (StakingCredential(StakingHash, StakingPtr)) as Types
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash(ValidatorHash)) as Types
import Data.Array (nub) as Array
import Data.ArrayBuffer.Typed (fromArray) as ArrayBuffer
import Data.ArrayBuffer.Typed.Gen (genUint8) as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UInt as UInt
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Plutus (TxInInfo(TxInInfo), Redeemer(Redeemer), Datum(Datum))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary) as Q
import Test.QuickCheck.Gen (Gen, chooseInt) as Q

genBool :: Q.Gen Boolean
genBool = Q.arbitrary

genInteger :: Q.Gen BigInt
genInteger = BigInt.fromInt <$> Q.chooseInt (-100000) 100000

genPlutusBytes :: Int -> Q.Gen ByteArray
genPlutusBytes len = ByteArray <$> genBytes len

genPlutusBytes' :: Int -> Q.Gen ByteArray
genPlutusBytes' maxLen = ByteArray <$> genBytes' maxLen

genCurrencySymbol :: Q.Gen Types.CurrencySymbol
genCurrencySymbol = unsafePartial (fromJust <<< Types.mkCurrencySymbol) <$> genPlutusBytes 28

genTokenName :: Q.Gen Types.TokenName
genTokenName = unsafePartial (fromJust <<< Types.mkTokenName) <$> genPlutusBytes' 32

genAmount :: Q.Gen BigInt
genAmount = BigInt.fromInt <$> Q.chooseInt 0 100_000

genValue :: Q.Gen Types.Value
genValue = genValue' <|> Value.singleton Types.adaSymbol Types.adaToken <$> genAmount
  where
  genValue' :: Q.Gen Types.Value
  genValue' = do
    currencySymbol <- genCurrencySymbol
    values <- genArray 10 (Value.singleton currencySymbol <$> genTokenName <*> genAmount)
    pure $ mconcat values

genMap :: forall k v. Eq k => Ord k => Q.Gen k -> Q.Gen v -> Q.Gen (Map.Map k v)
genMap gk gv =
  Map.Map
    <$> do
        keys <- Array.nub <$> genArray 5 gk
        (\k -> (Tuple k) <$> gv) `traverse` keys

genData :: Q.Gen PlutusData.PlutusData
genData = genData' 4
  where
  genData' :: Int -> Q.Gen PlutusData.PlutusData
  genData' 0 = PlutusData.Integer <$> genInteger <|> (PlutusData.Bytes <$> genPlutusBytes' 10)

  genData' depth =
    PlutusData.Integer <$> genInteger
      <|> PlutusData.Bytes
      <$> genPlutusBytes' 10
      <|> PlutusData.List
      <$> genArray 5 (genData' (depth - 1))
      <|> PlutusData.Map
      <$> genArray 5 (Tuple <$> genData' (depth - 1) <*> genData' (depth - 1))
      <|> PlutusData.Constr
      <$> (BigNum.fromInt <$> Q.chooseInt 0 5)
      <*> genArray 5 (genData' (depth - 1))

genDatum :: Q.Gen Datum
genDatum = Datum <$> genData

genRedeemer :: Q.Gen Redeemer
genRedeemer = Redeemer <$> genData

genPubKeyHash :: Q.Gen Types.PubKeyHash
genPubKeyHash = Types.PubKeyHash <$> Q.arbitrary

genScriptHash :: Q.Gen Types.ScriptHash
genScriptHash = Q.arbitrary

-- genRedeemerHash :: Q.Gen Types.RedeemerHash
-- genRedeemerHash = Types.RedeemerHash <$> genPlutusBytes 32
genDatumHash :: Q.Gen Types.DataHash
genDatumHash = Q.arbitrary

genExtended :: Q.Gen (Time.Extended Time.POSIXTime)
genExtended = pure Time.NegInf <|> pure Time.PosInf <|> Time.Finite <$> genPosixTime

genPosixTime :: Q.Gen Time.POSIXTime
genPosixTime = Time.POSIXTime <<< BigInt.fromInt <$> Q.chooseInt 0 1_000_000

genClosure :: Q.Gen Time.Closure
genClosure = genBool

genUpperBound :: Q.Gen (Time.UpperBound Time.POSIXTime)
genUpperBound = Time.UpperBound <$> genExtended <*> genClosure

genLowerBound :: Q.Gen (Time.LowerBound Time.POSIXTime)
genLowerBound = Time.LowerBound <$> genExtended <*> genClosure

genInterval :: Q.Gen (Time.Interval Time.POSIXTime)
genInterval =
  (genPosixTime >>= \pt -> pure $ Time.FiniteInterval pt (wrap $ (unwrap pt) + (BigInt.fromInt 100)))
    <|> Time.StartAt
    <$> genPosixTime
    <|> Time.EndAt
    <$> genPosixTime
    <|> pure Time.AlwaysInterval
    <|> pure Time.EmptyInterval

genAddress :: Q.Gen Types.Address
genAddress =
  Types.Address
    <$> do
        addressCredential <- genCredential
        addressStakingCredential <- pure Nothing <|> Just <$> genStakingCredential
        pure { addressCredential, addressStakingCredential }

genStakingCredential :: Q.Gen Types.StakingCredential
genStakingCredential =
  Types.StakingHash <$> genCredential
    <|> Types.StakingPtr
    <$> ( do
          slot <- BigInt.fromInt <$> Q.chooseInt 0 100_000
          txIx <- BigInt.fromInt <$> Q.chooseInt 0 100_000
          certIx <- BigInt.fromInt <$> Q.chooseInt 0 100_000
          pure { slot, txIx, certIx }
      )

genCredential :: Q.Gen Types.Credential
genCredential =
  Types.PubKeyCredential <$> genPubKeyHash
    <|> Types.ScriptCredential
    <<< Types.ValidatorHash
    <$> genScriptHash

genTxId :: Q.Gen Types.TransactionHash
genTxId = Q.arbitrary

genTxOutRef :: Q.Gen Types.TransactionInput
genTxOutRef = Types.TransactionInput <$> ({ transactionId: _, index: _ } <$> genTxId <*> (UInt.fromInt <$> Q.chooseInt 0 100_000))

genOutputDatum :: Q.Gen Types.OutputDatum
genOutputDatum =
  pure Types.NoOutputDatum
    <|> Types.OutputDatumHash
    <$> genDatumHash
    <|> Types.OutputDatum
    <$> genData

genTxOut :: Q.Gen Types.TransactionOutput
genTxOut = Types.TransactionOutput <$> ({ address: _, amount: _, datum: _, referenceScript: _ } <$> genAddress <*> genValue <*> genOutputDatum <*> (pure Nothing <$> Just <$> genScriptHash))

genTxInInfo :: Q.Gen TxInInfo
genTxInInfo = TxInInfo <$> ({ outRef: _, resolved: _ } <$> genTxOutRef <*> genTxOut)

-- | Utils
genBytes :: Int -> Q.Gen Uint8Array
genBytes len = do
  uint8s <- replicateA len (ArrayBuffer.genUint8)
  pure (unsafePerformEffect $ ArrayBuffer.fromArray uint8s)

genBytes' :: Int -> Q.Gen Uint8Array
genBytes' maxLen = do
  len <- Q.chooseInt 0 maxLen
  uint8s <- replicateA len (ArrayBuffer.genUint8)
  pure (unsafePerformEffect $ ArrayBuffer.fromArray uint8s)

genArray :: forall a. Int -> Q.Gen a -> Q.Gen (Array a)
genArray maxLen gen = do
  len <- Q.chooseInt 0 maxLen
  replicateA len gen
