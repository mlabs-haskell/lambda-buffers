{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Json.Plutus () where

import Control.Monad (unless)
import Data.Aeson (object, withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BSS
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import LambdaBuffers.Runtime.Json (Json (fromJson, toJson), caseJsonConstructor, jsonConstructor, (.:), (.=))
import PlutusLedgerApi.V1 (BuiltinByteString)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx.AssocMap qualified as PlutusTx

-- | lbf-prelude.Plutus.Json instance rule implementations for lbf-plutus package
instance Json PlutusV1.AssetClass where
  toJson (PlutusV1.AssetClass (policyId, tn)) = object ["currency_symbol" .= policyId, "token_name" .= tn]
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

-- ByteString representing the currency, hashed with BLAKE2b-224. It is empty for Ada, 28 bytes for MintingPolicyHash. Forms an AssetClass along with TokenName. A Value is a map from CurrencySymbol's to a map from TokenName to an Integer.
instance Json PlutusV1.CurrencySymbol where
  toJson (PlutusV1.CurrencySymbol plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.CurrencySymbol" $ PlutusV1.CurrencySymbol <$> decodeSizedPlutusBytes 28 v

-- ByteString of a name of a token. Shown as UTF-8 string when possible. Should be no longer than 32 bytes, empty for Ada. Forms an AssetClass along with a CurrencySymbol.
instance Json PlutusV1.TokenName where
  toJson (PlutusV1.TokenName plutusBytes) = toJson plutusBytes
  fromJson v =
    prependFailure "Plutus.V1.TokenName" $
      PlutusV1.TokenName <$> do
        plutusBytes <- fromJson @PlutusV1.BuiltinByteString v
        unless ((BSS.length . PlutusV1.fromBuiltin $ plutusBytes) <= 32) $ fail $ "Expected a Plutus TokenName of size smaller than or equal to 32 but got " <> show plutusBytes
        return plutusBytes

instance Json PlutusV1.Value where
  toJson (PlutusV1.Value currencyMap) = toJson currencyMap
  fromJson v = prependFailure "Plutus.V1.Value" (PlutusV1.Value <$> fromJson @(PlutusTx.Map PlutusV1.CurrencySymbol (PlutusTx.Map PlutusV1.TokenName Integer)) v)

instance (Json k, Json v) => Json (PlutusTx.Map k v) where
  toJson = toJson . PlutusTx.toList
  fromJson v = prependFailure "Plutus.V1.Map" $ PlutusTx.fromList <$> fromJson @[(k, v)] v

-- The hash of a public key. This is frequently used to identify the public key, rather than the key itself. Hashed with BLAKE2b-224. 28 bytes.
instance Json PlutusV1.PubKeyHash where
  toJson (PlutusV1.PubKeyHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.PubKeyHash" $ PlutusV1.PubKeyHash <$> decodeSizedPlutusBytes 28 v

-- Script runtime representation of a Digest SHA256.
instance Json PlutusV1.ScriptHash where
  toJson (PlutusV1.ScriptHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.ScriptHash" $ PlutusV1.ScriptHash <$> decodeSizedPlutusBytes 32 v

-- Type representing the BLAKE2b-256 hash of a redeemer. 32 bytes.
instance Json PlutusV1.RedeemerHash where
  toJson (PlutusV1.RedeemerHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.RedeemerHash" $ PlutusV1.RedeemerHash <$> decodeSizedPlutusBytes 32 v

-- Script runtime representation of a Digest SHA256.
instance Json PlutusV1.DatumHash where
  toJson (PlutusV1.DatumHash plutusBytes) = toJson plutusBytes
  fromJson v = prependFailure "Plutus.V1.DatumHash" $ PlutusV1.DatumHash <$> decodeSizedPlutusBytes 32 v

instance Json PlutusV1.Datum where
  toJson (PlutusV1.Datum plutusData) = toJson plutusData
  fromJson v = prependFailure "Plutus.V1.Datum" $ PlutusV1.Datum <$> fromJson @PlutusV1.BuiltinData v

instance Json PlutusV1.Redeemer where
  toJson (PlutusV1.Redeemer plutusData) = toJson plutusData
  fromJson v = prependFailure "Plutus.V1.Redeemer" $ PlutusV1.Redeemer <$> fromJson @PlutusV1.BuiltinData v

instance Json PlutusV1.Data where
  toJson (PlutusV1.I i) = jsonConstructor "Integer" [toJson i]
  toJson (PlutusV1.B b) = jsonConstructor "Bytes" [toJson $ PlutusV1.toBuiltin b]
  toJson (PlutusV1.List xs) = jsonConstructor "List" [toJson xs]
  toJson (PlutusV1.Map elems) = jsonConstructor "Map" [toJson elems]
  toJson (PlutusV1.Constr ctorIx prod) = jsonConstructor "Constr" [object ["index" .= toJson ctorIx, "fields" .= prod]]
  fromJson =
    caseJsonConstructor
      "Plutus.V1.Data"
      [
        ( "Integer"
        , \case
            [json] -> PlutusV1.I <$> fromJson @Integer json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ,
        ( "Bytes"
        , \case
            [json] -> PlutusV1.B . PlutusV1.fromBuiltin <$> fromJson @BuiltinByteString json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ,
        ( "List"
        , \case
            [json] -> PlutusV1.List <$> fromJson @[PlutusV1.Data] json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ,
        ( "Map"
        , \case
            [json] -> PlutusV1.Map <$> fromJson @[(PlutusV1.Data, PlutusV1.Data)] json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ,
        ( "Constr"
        , \case
            [json] ->
              withObject
                ""
                ( \obj -> do
                    ctorIx <- obj .: "index" >>= fromJson @Integer
                    ctorFields <- obj .: "fields" >>= fromJson @[PlutusV1.Data]
                    return $ PlutusV1.Constr ctorIx ctorFields
                )
                json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ]

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
  toJson PlutusV1.NegInf = jsonConstructor "NegInf" []
  toJson PlutusV1.PosInf = jsonConstructor "PosInf" []
  toJson (PlutusV1.Finite f) = jsonConstructor "Finite" [toJson f]
  fromJson =
    caseJsonConstructor
      "Plutus.V1.Extended"
      [
        ( "NegInf"
        , \case
            [] -> return PlutusV1.NegInf
            invalid -> fail $ "Expected a JSON Array with 0 elements but got " <> show invalid
        )
      ,
        ( "PosInf"
        , \case
            [] -> return PlutusV1.PosInf
            invalid -> fail $ "Expected a JSON Array with 0 elements but got " <> show invalid
        )
      ,
        ( "Finite"
        , \case
            [json] -> PlutusV1.Finite <$> fromJson @a json
            invalid -> fail $ "Expected a JSON Array with 1 element but got " <> show invalid
        )
      ]

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
  toJson (PlutusV1.PubKeyCredential pkh) = jsonConstructor "PubKeyCredential" [toJson pkh]
  toJson (PlutusV1.ScriptCredential scrh) = jsonConstructor "ScriptCredential" [toJson scrh]
  fromJson =
    caseJsonConstructor
      "Plutus.V1.Credential"
      [
        ( "PubKeyCredential"
        , \case
            [json] -> PlutusV1.PubKeyCredential <$> fromJson @PlutusV1.PubKeyHash json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ,
        ( "ScriptCredential"
        , \case
            [json] -> PlutusV1.ScriptCredential <$> fromJson @PlutusV1.ScriptHash json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ]

instance Json PlutusV1.StakingCredential where
  toJson (PlutusV1.StakingHash cred) = jsonConstructor "StakingHash" [toJson cred]
  toJson (PlutusV1.StakingPtr slot txIx certIx) =
    jsonConstructor
      "StakingPtr"
      [ object ["slot_number" .= toJson slot, "transaction_index" .= toJson txIx, "certificate_index" .= toJson certIx]
      ]
  fromJson =
    caseJsonConstructor
      "Plutus.V1.StakingCredential"
      [
        ( "StakingHash"
        , \case
            [json] -> PlutusV1.StakingHash <$> fromJson @PlutusV1.Credential json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ,
        ( "StakingPtr"
        , \case
            [json] ->
              withObject
                ""
                ( \obj -> do
                    slot <- obj .: "slot_number"
                    txIx <- obj .: "transaction_index"
                    certIx <- obj .: "certificate_index"
                    return $ PlutusV1.StakingPtr slot txIx certIx
                )
                json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ]

-- A transaction ID, i.e. the hash of a transaction. Hashed with BLAKE2b-256. 32 byte.
instance Json PlutusV1.TxId where
  toJson (PlutusV1.TxId txId) = toJson txId
  fromJson v = prependFailure "Plutus.V1.TxId" (PlutusV1.TxId <$> decodeSizedPlutusBytes 32 v)

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

instance Json PlutusV2.TxOut where
  toJson (PlutusV2.TxOut addr val dat mayRefScript) = object ["address" .= toJson addr, "value" .= toJson val, "datum" .= toJson dat, "reference_script" .= toJson mayRefScript]
  fromJson =
    withObject
      "Plutus.V2.TxOut"
      ( \obj -> do
          addr <- obj .: "address"
          val <- obj .: "value"
          dat <- obj .: "datum"
          mayRefScript <- obj .: "reference_script"
          return $ PlutusV2.TxOut addr val dat mayRefScript
      )

instance Json PlutusV2.OutputDatum where
  toJson PlutusV2.NoOutputDatum = jsonConstructor "NoOutputDatum" []
  toJson (PlutusV2.OutputDatumHash dh) = jsonConstructor "OutputDatumHash" [toJson dh]
  toJson (PlutusV2.OutputDatum dat) = jsonConstructor "OutputDatum" [toJson dat]
  fromJson =
    caseJsonConstructor
      "Plutus.V2.OutputDatum"
      [
        ( "NoOutputDatum"
        , \case
            [] -> return PlutusV2.NoOutputDatum
            invalid -> fail $ "Expected a JSON Array with 0 elements but got " <> show invalid
        )
      ,
        ( "OutputDatumHash"
        , \case
            [json] -> PlutusV2.OutputDatumHash <$> fromJson json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ,
        ( "OutputDatum"
        , \case
            [json] -> PlutusV2.OutputDatum <$> fromJson json
            invalid -> fail $ "Expected a JSON Array with 1 elements but got " <> show invalid
        )
      ]

instance Json PlutusV2.TxInInfo where
  toJson (PlutusV2.TxInInfo outRef out) = object ["reference" .= toJson outRef, "output" .= toJson out]
  fromJson =
    withObject
      "Plutus.V2.TxInInfo"
      ( \obj -> do
          outRef <- obj .: "reference"
          out <- obj .: "output"
          return $ PlutusV2.TxInInfo outRef out
      )

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = Text.decodeUtf8 . Base16.encode

decodeByteString :: MonadFail m => Text -> m BSS.ByteString
decodeByteString = either (\err -> fail $ "[LambdaBuffers.Runtime.Json.Plutus] Failed decoding from base16 into bytes " <> err) pure . tryDecode
  where
    tryDecode :: Text.Text -> Either String BSS.ByteString
    tryDecode = Base16.decode . Text.encodeUtf8

decodeSizedPlutusBytes :: Int -> Aeson.Value -> Aeson.Parser PlutusV1.BuiltinByteString
decodeSizedPlutusBytes size json = do
  bytes <- fromJson @PlutusV1.BuiltinByteString json
  unless ((BSS.length . PlutusV1.fromBuiltin @_ @BSS.ByteString $ bytes) == size) $ fail $ "Expected Plutus Bytes of size " <> show size <> " but got: " <> show bytes
  return bytes
