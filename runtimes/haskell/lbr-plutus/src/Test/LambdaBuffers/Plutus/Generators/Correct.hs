module Test.LambdaBuffers.Plutus.Generators.Correct (
  genValue,
  genCurrencySymbol,
  genTokenName,
  genAssetClass,
  genData,
  genPubKeyHash,
  genScriptHash,
  genRedeemerHash,
  genDatumHash,
  genInterval,
  genLowerBound,
  genUpperBound,
  genExtended,
  genPosixTime,
  genClosure,
  genAddress,
  genStakingCredential,
  genCredential,
  genTxId,
  genTxOutRef,
  genRedeemer,
  genDatum,
  genOutputDatum,
  genTxOut,
  genTxInInfo,
) where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx.AssocMap qualified as PlutusTx

-- | Default constant range used in various generators
defRange :: HR.Range Int
defRange = HR.constant 0 5

genAssetClass :: H.Gen PlutusV1.AssetClass
genAssetClass = PlutusV1.AssetClass <$> genPair genCurrencySymbol genTokenName

genCurrencySymbol :: H.Gen PlutusV1.CurrencySymbol
genCurrencySymbol = PlutusV1.CurrencySymbol <$> genPlutusBytes 28

genTokenName :: H.Gen PlutusV1.TokenName
genTokenName = PlutusV1.TokenName <$> genPlutusBytes' 32

genAmount :: H.Gen Integer
genAmount = H.integral (HR.constant (-100) 100)

genValue :: H.Gen PlutusV1.Value
genValue =
  PlutusV1.Value
    <$> H.choice
      [ genMap genCurrencySymbol (genMap genTokenName genAmount)
      , genMap (return PlutusV1.adaSymbol) (genMap (return PlutusV1.adaToken) genAmount)
      ]

genMap :: (Eq k) => H.Gen k -> H.Gen v -> H.Gen (PlutusTx.Map k v)
genMap gk gv =
  PlutusTx.fromList <$> do
    keys <- List.nub <$> H.list defRange gk
    (\k -> (k,) <$> gv) `traverse` keys

genPair :: H.Gen x -> H.Gen y -> H.Gen (x, y)
genPair x y = (,) <$> x <*> y

genPlutusBytes :: Int -> H.Gen PlutusV1.BuiltinByteString
genPlutusBytes len = PlutusV1.toBuiltin <$> genBytes len

genBytes :: Int -> H.Gen ByteString
genBytes len = H.bytes (HR.singleton len)

genPlutusBytes' :: Int -> H.Gen PlutusV1.BuiltinByteString
genPlutusBytes' maxLen = PlutusV1.toBuiltin <$> genBytes' maxLen

genBytes' :: Int -> H.Gen ByteString
genBytes' maxLen = H.bytes (HR.constant 0 maxLen)

genData :: H.Gen PlutusV1.Data
genData = genData' (5 :: Integer)
  where
    genData' :: Integer -> H.Gen PlutusV1.Data
    genData' 0 =
      H.choice
        [ PlutusV1.I <$> H.integral (HR.constant (-1000000000000) 1000000000000)
        , PlutusV1.B <$> genBytes' 10
        ]
    genData' depth =
      H.choice
        [ PlutusV1.I <$> H.integral (HR.constant (-1000000000000) 1000000000000)
        , PlutusV1.B <$> genBytes' 10
        , PlutusV1.List <$> H.list (HR.constant 0 5) (genData' (depth - 1))
        , PlutusV1.Map <$> H.list (HR.constant 0 5) ((,) <$> genData' (depth - 1) <*> genData' (depth - 1))
        , PlutusV1.Constr <$> H.integral (HR.constant 0 5) <*> H.list (HR.constant 0 5) (genData' (depth - 1))
        ]

genPubKeyHash :: H.Gen PlutusV1.PubKeyHash
genPubKeyHash = PlutusV1.PubKeyHash <$> genPlutusBytes 28

genScriptHash :: H.Gen PlutusV1.ScriptHash
genScriptHash = PlutusV1.ScriptHash <$> genPlutusBytes 28

genRedeemerHash :: H.Gen PlutusV1.RedeemerHash
genRedeemerHash = PlutusV1.RedeemerHash <$> genPlutusBytes 32

genDatumHash :: H.Gen PlutusV1.DatumHash
genDatumHash = PlutusV1.DatumHash <$> genPlutusBytes 32

genExtended :: H.Gen (PlutusV1.Extended PlutusV1.POSIXTime)
genExtended =
  H.choice
    [ return PlutusV1.NegInf
    , return PlutusV1.PosInf
    , PlutusV1.Finite <$> genPosixTime
    ]

genPosixTime :: H.Gen PlutusV1.POSIXTime
genPosixTime = PlutusV1.POSIXTime <$> H.integral (HR.constant 0 1000000)

genClosure :: H.Gen PlutusV1.Closure
genClosure = H.bool

genUpperBound :: H.Gen (PlutusV1.UpperBound PlutusV1.POSIXTime)
genUpperBound = PlutusV1.UpperBound <$> genExtended <*> genClosure

genLowerBound :: H.Gen (PlutusV1.LowerBound PlutusV1.POSIXTime)
genLowerBound = PlutusV1.LowerBound <$> genExtended <*> genClosure

genInterval :: H.Gen (PlutusV1.Interval PlutusV1.POSIXTime)
genInterval = PlutusV1.Interval <$> genLowerBound <*> genUpperBound

genAddress :: H.Gen PlutusV1.Address
genAddress = PlutusV1.Address <$> genCredential <*> H.choice [return Nothing, Just <$> genStakingCredential]

genStakingCredential :: H.Gen PlutusV1.StakingCredential
genStakingCredential =
  H.choice
    [ PlutusV1.StakingHash <$> genCredential
    , PlutusV1.StakingPtr <$> H.integral (HR.constant 0 1000000) <*> H.integral (HR.constant 0 1000000) <*> H.integral (HR.constant 0 1000000)
    ]

genCredential :: H.Gen PlutusV1.Credential
genCredential =
  H.choice
    [ PlutusV1.PubKeyCredential <$> genPubKeyHash
    , PlutusV1.ScriptCredential <$> genScriptHash
    ]

genTxId :: H.Gen PlutusV1.TxId
genTxId = PlutusV1.TxId <$> genPlutusBytes 32

genTxOutRef :: H.Gen PlutusV1.TxOutRef
genTxOutRef = PlutusV1.TxOutRef <$> genTxId <*> H.integral (HR.constant 0 1000000)

genTxOut :: H.Gen PlutusV2.TxOut
genTxOut = PlutusV2.TxOut <$> genAddress <*> genValue <*> genOutputDatum <*> H.choice [return Nothing, Just <$> genScriptHash]

genTxInInfo :: H.Gen PlutusV2.TxInInfo
genTxInInfo = PlutusV2.TxInInfo <$> genTxOutRef <*> genTxOut

genOutputDatum :: H.Gen PlutusV2.OutputDatum
genOutputDatum =
  H.choice
    [ return PlutusV2.NoOutputDatum
    , PlutusV2.OutputDatumHash <$> genDatumHash
    , PlutusV2.OutputDatum <$> genDatum
    ]

genDatum :: H.Gen PlutusV1.Datum
genDatum = PlutusV1.Datum . PlutusV1.dataToBuiltinData <$> genData

genRedeemer :: H.Gen PlutusV1.Redeemer
genRedeemer = PlutusV1.Redeemer . PlutusV1.dataToBuiltinData <$> genData
