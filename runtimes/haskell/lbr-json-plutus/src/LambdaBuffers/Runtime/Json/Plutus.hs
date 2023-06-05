{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Json.Plutus () where

import Data.Aeson (object, withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (prependFailure)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BSS
import Data.ByteString.Base16 qualified as Base16
import Data.Foldable (Foldable (toList))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import PlutusLedgerApi.V1 (BuiltinByteString)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusTx.AssocMap qualified as PlutusTx

class Json a where
  toJson :: a -> Aeson.Value
  fromJson :: Aeson.Value -> Aeson.Parser a

type Key = String

(.=) :: Json v => Key -> v -> Aeson.Pair
(.=) k v = (Key.fromString k, toJson v)

(.:) :: forall a. Json a => Aeson.Object -> Key -> Aeson.Parser a
(.:) obj k = Aeson.explicitParseField (fromJson @a) obj (Key.fromString k)

toJsonConstructor :: forall {a}. Json a => Text -> a -> Aeson.Value
toJsonConstructor ctorName ctorProduct = object ["ctor_name" .= Aeson.String ctorName, "ctor_product" .= toJson ctorProduct]

fromJsonConstructor :: (Text -> Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
fromJsonConstructor f =
  Aeson.withObject
    "[LambdaBuffers.Runtime.Json.Plutus][fromJsonConstructor]"
    ( \obj -> do
        ctorName <- obj .: "ctor_name" >>= Aeson.withText "[LambdaBuffers.Runtime.Json.Plutus][ctor_name]" return
        ctorProduct <- obj .: "ctor_product"
        f ctorName ctorProduct
    )

instance Json PlutusV1.AssetClass where
  toJson (PlutusV1.AssetClass (policyId, tn)) = object ["policy_id" .= policyId, "token_name" .= tn]
  fromJson =
    withObject
      "Plutus.V1.AssetClass"
      ( \obj ->
          curry PlutusV1.AssetClass
            <$> obj
              .: "currency_symbol"
            <*> obj
              .: "token_name"
      )

instance Json PlutusV1.CurrencySymbol where
  toJson (PlutusV1.CurrencySymbol plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.CurrencySymbol" $ PlutusV1.CurrencySymbol <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.TokenName where
  toJson (PlutusV1.TokenName plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.TokenName" $ PlutusV1.TokenName <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.Value where
  toJson (PlutusV1.Value currencyMap) = toJson currencyMap
  fromJson v = prependFailure "Plutus.V1.Value" (PlutusV1.Value <$> fromJson @(PlutusTx.Map PlutusV1.CurrencySymbol (PlutusTx.Map PlutusV1.TokenName Integer)) v)
instance (Json k, Json v) => Json (PlutusTx.Map k v) where
  toJson = toJson . PlutusTx.toList
  fromJson v = prependFailure "Plutus.V1.Map" $ PlutusTx.fromList <$> fromJson @[(k, v)] v

instance Json PlutusV1.PubKeyHash where
  toJson (PlutusV1.PubKeyHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.PubKeyHash" $ PlutusV1.PubKeyHash <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.ScriptHash where
  toJson (PlutusV1.ScriptHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.ScriptHash" $ PlutusV1.ScriptHash <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.RedeemerHash where
  toJson (PlutusV1.RedeemerHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.RedeemerHash" $ PlutusV1.RedeemerHash <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.DatumHash where
  toJson (PlutusV1.DatumHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.DatumHash" $ PlutusV1.DatumHash <$> fromJson @PlutusV1.BuiltinByteString v

instance Json PlutusV1.Datum where
  toJson (PlutusV1.Datum plutusData) = toJson plutusData
  fromJson v = prependFailure "Plutus.V1.Datum" $ PlutusV1.Datum <$> fromJson @PlutusV1.BuiltinData v

instance Json PlutusV1.Redeemer where
  toJson (PlutusV1.Redeemer plutusData) = toJson plutusData
  fromJson v = prependFailure "Plutus.V1.Redeemer" $ PlutusV1.Redeemer <$> fromJson @PlutusV1.BuiltinData v

instance Json PlutusV1.Data where
  toJson (PlutusV1.I i) = toJsonConstructor "Integer" i
  toJson (PlutusV1.B b) = toJsonConstructor "Bytes" (PlutusV1.toBuiltin b)
  toJson (PlutusV1.List xs) = toJsonConstructor "List" xs
  toJson (PlutusV1.Map elems) = toJsonConstructor "Map" elems
  toJson (PlutusV1.Constr ctorIx prod) = toJsonConstructor "Constr" (object ["index" .= toJson ctorIx, "product" .= prod])
  fromJson v =
    prependFailure "Plutus.V1.Data" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "Integer" -> prependFailure "Integer" $ PlutusV1.I <$> fromJson @Integer ctorProduct
            "Bytes" -> prependFailure "Bytes" $ PlutusV1.B . PlutusV1.fromBuiltin <$> fromJson @BuiltinByteString ctorProduct
            "List" -> prependFailure "List" $ PlutusV1.List <$> fromJson @[PlutusV1.Data] ctorProduct
            "Map" -> prependFailure "Map" $ PlutusV1.Map <$> fromJson @[(PlutusV1.Data, PlutusV1.Data)] ctorProduct
            "Constr" ->
              Aeson.withObject
                "Constr"
                ( \obj -> do
                    ctorIx <- obj .: "index" >>= fromJson @Integer
                    ctorProd <- obj .: "product" >>= fromJson @[PlutusV1.Data]
                    return (PlutusV1.Constr ctorIx ctorProd)
                )
                ctorProduct
            invalid -> fail $ "Received a an invalid constructor name " <> Text.unpack invalid
        )
        v

instance Json PlutusV1.BuiltinData where
  toJson (PlutusV1.BuiltinData d) = toJson d
  fromJson v = PlutusV1.BuiltinData <$> fromJson @PlutusV1.Data v

instance Json PlutusV1.BuiltinByteString where
  toJson plutusBytes = Aeson.String . encodeByteString $ PlutusV1.fromBuiltin plutusBytes
  fromJson = Aeson.withText "Plutus.V1.Bytes" (fmap PlutusV1.toBuiltin . decodeByteString)

instance Json PlutusV1.LedgerBytes where
  toJson (PlutusV1.LedgerBytes plutusBytes) = Aeson.String $ encodeByteString $ PlutusV1.fromBuiltin plutusBytes
  fromJson = Aeson.withText "Plutus.V1.Bytes" (fmap (PlutusV1.LedgerBytes . PlutusV1.toBuiltin) . decodeByteString)

instance Json a => Json (PlutusV1.Extended a) where
  toJson PlutusV1.NegInf = toJsonConstructor "NegInf" ()
  toJson PlutusV1.PosInf = toJsonConstructor "PosInf" ()
  toJson (PlutusV1.Finite f) = toJsonConstructor "Finite" f
  fromJson v =
    prependFailure "Plutus.V1.Extended" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "NegInf" -> prependFailure "NegInf" $ fromJson @() ctorProduct >> return PlutusV1.NegInf
            "PosInf" -> prependFailure "PosInf" $ fromJson @() ctorProduct >> return PlutusV1.PosInf
            "Finite" -> prependFailure "Finite" $ PlutusV1.Finite <$> fromJson @a ctorProduct
            invalid -> fail $ "Received a an invalid constructor name " <> Text.unpack invalid
        )
        v

instance Json a => Json (PlutusV1.UpperBound a) where
  toJson (PlutusV1.UpperBound bound closed) = object ["bound" .= toJson bound, "closed" .= toJson closed]
  fromJson =
    withObject
      "Plutus.V1.UpperBound"
      ( \obj -> do
          bound <- obj .: "bound"
          closed <- obj .: "closed"
          return $ PlutusV1.UpperBound bound closed
      )

instance Json a => Json (PlutusV1.LowerBound a) where
  toJson (PlutusV1.LowerBound bound closed) = object ["bound" .= toJson bound, "closed" .= toJson closed]
  fromJson =
    withObject
      "Plutus.V1.LowerBound"
      ( \obj -> do
          bound <- obj .: "bound"
          closed <- obj .: "closed"
          return $ PlutusV1.LowerBound bound closed
      )

instance Json a => Json (PlutusV1.Interval a) where
  toJson (PlutusV1.Interval from to) = object ["from" .= toJson from, "to" .= toJson to]
  fromJson =
    withObject
      "Plutus.V1.Interval"
      ( \obj -> do
          from <- obj .: "from"
          to <- obj .: "to"
          return $ PlutusV1.Interval from to
      )

instance Json PlutusV1.POSIXTime where
  toJson (PlutusV1.POSIXTime t) = toJson t
  fromJson v = prependFailure "Plutus.V1.POSIXTime" (PlutusV1.POSIXTime <$> fromJson @Integer v)

instance Json PlutusV1.Address where
  toJson (PlutusV1.Address cred stakingCred) = object ["credential" .= toJson cred, "staking_credential" .= toJson stakingCred]
  fromJson =
    withObject
      "Plutus.V1.Address"
      ( \obj -> do
          cred <- obj .: "credential"
          stakingCred <- obj .: "staking_credential"
          return $ PlutusV1.Address cred stakingCred
      )

instance Json PlutusV1.Credential where
  toJson (PlutusV1.PubKeyCredential pkh) = toJsonConstructor "PubKeyCredential" pkh
  toJson (PlutusV1.ScriptCredential scrh) = toJsonConstructor "ScriptCredential" scrh
  fromJson v =
    prependFailure "Plutus.V1.Credential" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "PubKeyCredential" -> prependFailure "PubKeyCredential" $ PlutusV1.PubKeyCredential <$> fromJson @PlutusV1.PubKeyHash ctorProduct
            "ScriptCredential" -> prependFailure "ScriptCredential" $ PlutusV1.ScriptCredential <$> fromJson @PlutusV1.ScriptHash ctorProduct
            invalid -> fail $ "Received a an invalid constructor name " <> Text.unpack invalid
        )
        v

instance Json PlutusV1.StakingCredential where
  toJson (PlutusV1.StakingHash cred) = toJsonConstructor "StakingHash" cred
  toJson (PlutusV1.StakingPtr slot txIx certIx) = toJsonConstructor "ScriptCredential" (object ["slot_number" .= toJson slot, "transaction_index" .= toJson txIx, "certificate_index" .= toJson certIx])
  fromJson v =
    prependFailure "Plutus.V1.StakingCredential" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "StakingHash" -> prependFailure "StakingHash" $ PlutusV1.StakingHash <$> fromJson @PlutusV1.Credential ctorProduct
            "StakingPtr" ->
              withObject
                "StakingPtr"
                ( \obj -> do
                    slot <- obj .: "slot_number"
                    txIx <- obj .: "transaction_index"
                    certIx <- obj .: "certificate_index"
                    return $ PlutusV1.StakingPtr slot txIx certIx
                )
                ctorProduct
            invalid -> fail $ "Received a an invalid constructor name " <> Text.unpack invalid
        )
        v

instance Json PlutusV1.TxId where
  toJson (PlutusV1.TxId txId) = toJson txId
  fromJson v = prependFailure "Plutus.V1.TxId" (PlutusV1.TxId <$> fromJson @BuiltinByteString v)

instance Json PlutusV1.TxOutRef where
  toJson (PlutusV1.TxOutRef txId ix) = object ["transaction_id" .= toJson txId, "index" .= toJson ix]
  fromJson =
    withObject
      "Plutus.V1.TxOutRef"
      ( \obj -> do
          txId <- obj .: "transaction_id"
          index <- obj .: "index"
          return $ PlutusV1.TxOutRef txId index
      )

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = Text.decodeUtf8 . Base16.encode

decodeByteString :: MonadFail m => Text -> m BSS.ByteString
decodeByteString = either (\err -> fail $ "[LambdaBuffers.Runtime.Json.Plutus] Failed decoding from base16 into bytes " <> err) pure . tryDecode
  where
    tryDecode :: Text.Text -> Either String BSS.ByteString
    tryDecode = Base16.decode . Text.encodeUtf8

instance (Json a, Json b) => Json (a, b) where
  toJson (x, y) = toJson [toJson x, toJson y]
  fromJson =
    Aeson.withArray
      "[LambdaBuffers.Runtime.Json.Plutus][Json (a,b)]"
      ( \arr -> case toList arr of
          [x, y] -> do
            x' <- prependFailure "first" $ fromJson x
            y' <- prependFailure "second" $ fromJson y
            return (x', y')
          _ -> fail ""
      )

instance Json () where
  toJson _ = Aeson.Null
  fromJson v =
    if v == Aeson.Null
      then return ()
      else fail $ "[LambdaBuffers.Runtime.Json.Plutus][Json ()] Expected a null but got " <> show v

instance Json a => Json [a] where
  toJson = Aeson.Array . Vector.fromList . fmap toJson
  fromJson = Aeson.withArray "[LambdaBuffers.Runtime.Json.Plutus][Json [a]]" (fmap toList . traverse fromJson)

instance Json Aeson.Value where
  toJson v = v
  fromJson = return

instance Json Integer where
  toJson = Aeson.toJSON
  fromJson v = prependFailure "[LambdaBuffers.Runtime.Json.Plutus][Json Integer]" (Aeson.parseJSON v)

instance Json Bool where
  toJson = Aeson.toJSON
  fromJson v = prependFailure "[LambdaBuffers.Runtime.Json.Plutus][Json Bool]" (Aeson.parseJSON v)

instance Json a => Json (Maybe a) where
  toJson Nothing = toJsonConstructor "Nothing" ()
  toJson (Just x) = toJsonConstructor "Just" x
  fromJson v =
    prependFailure "[LambdaBuffers.Runtime.Json.Plutus][Json (Maybe a)]" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "Nothing" -> prependFailure "Nothing" $ fromJson @() ctorProduct >> return Nothing
            "Just" -> prependFailure "Just" $ Just <$> fromJson @a ctorProduct
            invalid -> fail $ "Received a an invalid constructor name " <> Text.unpack invalid
        )
        v
