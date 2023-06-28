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
  , genRedeemerHash
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
import Ctl.Internal.Plutus.Types.Address (Address(..)) as PlutusV1
import Ctl.Internal.Plutus.Types.AssocMap as PlutusV1.AssocMap
import Ctl.Internal.Plutus.Types.Credential (Credential(..), StakingCredential(..)) as PlutusV1
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol, adaSymbol, mkCurrencySymbol) as PlutusV1
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput(..)) as PlutusV2
import Ctl.Internal.Plutus.Types.Value (Value) as PlutusV1
import Ctl.Internal.Plutus.Types.Value as PlutusV1.Value
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashFromBytes) as PlutusV1
import Ctl.Internal.Serialization.Hash as Hash
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray(..)) as PlutusV1
import Ctl.Internal.Types.Datum (Datum(..)) as PlutusV1
import Ctl.Internal.Types.Interval (Closure, Extended(..), Interval(..), LowerBound(..), POSIXTime(..), UpperBound(..)) as PlutusV1
import Ctl.Internal.Types.OutputDatum (OutputDatum(..)) as PlutusV2
import Ctl.Internal.Types.PlutusData (PlutusData(..)) as PlutusV1
import Ctl.Internal.Types.PubKeyHash (PubKeyHash(..)) as PlutusV1
import Ctl.Internal.Types.Redeemer (Redeemer(..), RedeemerHash(..)) as PlutusV1
import Ctl.Internal.Types.Scripts (ValidatorHash(..)) as PlutusV1
import Ctl.Internal.Types.TokenName (TokenName, adaToken, mkTokenName) as PlutusV1
import Ctl.Internal.Types.Transaction (DataHash(..), TransactionHash(..), TransactionInput(..)) as PlutusV1
import Data.Array as Array
import Data.ArrayBuffer.Typed (fromArray) as ArrayBuffer
import Data.ArrayBuffer.Typed.Gen (genUint8) as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UInt as UInt
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Plutus as PlutusV2
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary) as Q
import Test.QuickCheck.Gen (Gen, chooseInt) as Q

genBool :: Q.Gen Boolean
genBool = Q.arbitrary

genInteger :: Q.Gen BigInt
genInteger = BigInt.fromInt <$> Q.chooseInt (-100000) 100000

genPlutusBytes :: Int -> Q.Gen PlutusV1.ByteArray
genPlutusBytes len = PlutusV1.ByteArray <$> genBytes len

genPlutusBytes' :: Int -> Q.Gen PlutusV1.ByteArray
genPlutusBytes' maxLen = PlutusV1.ByteArray <$> genBytes' maxLen

-- genAssetClass :: Q.Gen AssetClass
-- genAssetClass = AssetClass <$> genCurrencySymbol <*> genTokenName
genCurrencySymbol :: Q.Gen PlutusV1.CurrencySymbol
genCurrencySymbol = unsafePartial (fromJust <<< PlutusV1.mkCurrencySymbol) <$> genPlutusBytes 28

genTokenName :: Q.Gen PlutusV1.TokenName
genTokenName = unsafePartial (fromJust <<< PlutusV1.mkTokenName) <$> genPlutusBytes' 32

genAmount :: Q.Gen BigInt
genAmount = BigInt.fromInt <$> Q.chooseInt 0 100_000

genValue :: Q.Gen PlutusV1.Value
genValue = genValue' <|> PlutusV1.Value.singleton PlutusV1.adaSymbol PlutusV1.adaToken <$> genAmount
  where
  genValue' :: Q.Gen PlutusV1.Value
  genValue' = do
    currencySymbol <- genCurrencySymbol
    values <- genArray 10 (PlutusV1.Value.singleton currencySymbol <$> genTokenName <*> genAmount)
    pure $ mconcat values

genMap :: forall k v. Eq k => Ord k => Q.Gen k -> Q.Gen v -> Q.Gen (PlutusV1.AssocMap.Map k v)
genMap gk gv =
  PlutusV1.AssocMap.Map
    <$> do
        keys <- Array.nub <$> genArray 5 gk
        (\k -> (Tuple k) <$> gv) `traverse` keys

genData :: Q.Gen PlutusV1.PlutusData
genData = genData' 4
  where
  genData' :: Int -> Q.Gen PlutusV1.PlutusData
  genData' 0 = PlutusV1.Integer <$> genInteger <|> (PlutusV1.Bytes <$> genPlutusBytes' 10)

  genData' depth =
    PlutusV1.Integer <$> genInteger
      <|> PlutusV1.Bytes
      <$> genPlutusBytes' 10
      <|> PlutusV1.List
      <$> genArray 5 (genData' (depth - 1))
      <|> PlutusV1.Map
      <$> genArray 5 (Tuple <$> genData' (depth - 1) <*> genData' (depth - 1))
      <|> PlutusV1.Constr
      <$> (BigNum.fromInt <$> Q.chooseInt 0 5)
      <*> genArray 5 (genData' (depth - 1))

genDatum :: Q.Gen PlutusV1.Datum
genDatum = PlutusV1.Datum <$> genData

genRedeemer :: Q.Gen PlutusV1.Redeemer
genRedeemer = PlutusV1.Redeemer <$> genData

genPubKeyHash :: Q.Gen PlutusV1.PubKeyHash
genPubKeyHash = PlutusV1.PubKeyHash <<< unsafePartial (fromJust <<< Hash.ed25519KeyHashFromBytes) <$> genPlutusBytes 28

genScriptHash :: Q.Gen PlutusV1.ScriptHash
genScriptHash = unsafePartial (fromJust <<< PlutusV1.scriptHashFromBytes) <$> genPlutusBytes 28

genRedeemerHash :: Q.Gen PlutusV1.RedeemerHash
genRedeemerHash = PlutusV1.RedeemerHash <$> genPlutusBytes 32

genDatumHash :: Q.Gen PlutusV1.DataHash
genDatumHash = PlutusV1.DataHash <$> genPlutusBytes 32

genExtended :: Q.Gen (PlutusV1.Extended PlutusV1.POSIXTime)
genExtended = pure PlutusV1.NegInf <|> pure PlutusV1.PosInf <|> PlutusV1.Finite <$> genPosixTime

genPosixTime :: Q.Gen PlutusV1.POSIXTime
genPosixTime = PlutusV1.POSIXTime <<< BigInt.fromInt <$> Q.chooseInt 0 1_000_000

genClosure :: Q.Gen PlutusV1.Closure
genClosure = genBool

genUpperBound :: Q.Gen (PlutusV1.UpperBound PlutusV1.POSIXTime)
genUpperBound = PlutusV1.UpperBound <$> genExtended <*> genClosure

genLowerBound :: Q.Gen (PlutusV1.LowerBound PlutusV1.POSIXTime)
genLowerBound = PlutusV1.LowerBound <$> genExtended <*> genClosure

genInterval :: Q.Gen (PlutusV1.Interval PlutusV1.POSIXTime)
genInterval =
  (genPosixTime >>= \pt -> pure $ PlutusV1.FiniteInterval pt (wrap $ (unwrap pt) + (BigInt.fromInt 100)))
    <|> PlutusV1.StartAt
    <$> genPosixTime
    <|> PlutusV1.EndAt
    <$> genPosixTime
    <|> pure PlutusV1.AlwaysInterval
    <|> pure PlutusV1.EmptyInterval

genAddress :: Q.Gen PlutusV1.Address
genAddress =
  PlutusV1.Address
    <$> do
        addressCredential <- genCredential
        addressStakingCredential <- pure Nothing <|> Just <$> genStakingCredential
        pure { addressCredential, addressStakingCredential }

genStakingCredential :: Q.Gen PlutusV1.StakingCredential
genStakingCredential =
  PlutusV1.StakingHash <$> genCredential
    <|> PlutusV1.StakingPtr
    <$> ( do
          slot <- wrap <<< BigNum.fromInt <$> Q.chooseInt 0 100_000
          txIx <- wrap <<< BigNum.fromInt <$> Q.chooseInt 0 100_000
          certIx <- wrap <<< BigNum.fromInt <$> Q.chooseInt 0 100_000
          pure { slot, txIx, certIx }
      )

genCredential :: Q.Gen PlutusV1.Credential
genCredential =
  PlutusV1.PubKeyCredential <$> genPubKeyHash
    <|> PlutusV1.ScriptCredential
    <<< PlutusV1.ValidatorHash
    <$> genScriptHash

genTxId :: Q.Gen PlutusV1.TransactionHash
genTxId = PlutusV1.TransactionHash <$> genPlutusBytes 32

genTxOutRef :: Q.Gen PlutusV1.TransactionInput
genTxOutRef = PlutusV1.TransactionInput <$> ({ transactionId: _, index: _ } <$> genTxId <*> (UInt.fromInt <$> Q.chooseInt 0 100_000))

genOutputDatum :: Q.Gen PlutusV2.OutputDatum
genOutputDatum =
  pure PlutusV2.NoOutputDatum
    <|> PlutusV2.OutputDatumHash
    <$> genDatumHash
    <|> PlutusV2.OutputDatum
    <$> genDatum

genTxOut :: Q.Gen PlutusV2.TransactionOutput
genTxOut = PlutusV2.TransactionOutput <$> ({ address: _, amount: _, datum: _, referenceScript: _ } <$> genAddress <*> genValue <*> genOutputDatum <*> (pure Nothing <$> Just <$> genScriptHash))

genTxInInfo :: Q.Gen PlutusV2.TxInInfo
genTxInInfo = PlutusV2.TxInInfo <$> ({ outRef: _, resolved: _ } <$> genTxOutRef <*> genTxOut)

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
