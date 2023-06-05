{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Json.Plutus where

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

type Error = String

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
            invalid -> fail $ "[LambdaBuffers.Runtime.Json.Plutus][Json Data] Received a an invalid constructor name " <> Text.unpack invalid
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
            x' <- fromJson x
            y' <- fromJson y
            return (x', y')
          _ -> fail ""
      )

instance Json a => Json [a] where
  toJson = Aeson.Array . Vector.fromList . fmap toJson
  fromJson = Aeson.withArray "[LambdaBuffers.Runtime.Json.Plutus][Json [a]]" (fmap toList . traverse fromJson)

instance Json Aeson.Value where
  toJson v = v
  fromJson = return

instance Json Integer where
  fromJson v = prependFailure "[LambdaBuffers.Runtime.Json.Plutus][Json Integer]" (Aeson.parseJSON v)
  toJson = Aeson.toJSON
