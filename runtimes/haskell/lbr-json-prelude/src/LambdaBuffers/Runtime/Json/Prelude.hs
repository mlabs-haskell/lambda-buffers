module LambdaBuffers.Runtime.Json.Prelude (Json (toJson, fromJson), toJsonBytes, fromJsonBytes) where

import Data.Aeson (object, withArray, withObject, withText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEnc
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Aeson.Parser qualified as Aeson
import Data.Aeson.Types (prependFailure)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BSS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (Foldable (toList))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector

-- | Class that lbf-prelude.Prelude.Json class maps to from
class Json a where
  toJson :: a -> Aeson.Value
  fromJson :: Aeson.Value -> Aeson.Parser a

toJsonBytes :: Json a => a -> ByteString
toJsonBytes = LBS.toStrict . Aeson.encodingToLazyByteString . AesonEnc.value . toJson

fromJsonBytes :: forall a. Json a => ByteString -> Either String a
fromJsonBytes bs = case Aeson.eitherDecodeStrictWith Aeson.json (Aeson.iparse $ fromJson @a) bs of
  Left (jsonPath, err) -> Left $ "[" <> Aeson.formatPath jsonPath <> "] " <> err
  Right res -> Right res

-- | lbf-prelude.Prelude.Json instance rule implementations
instance Json Integer where
  toJson = Aeson.toJSON
  fromJson v = prependFailure "instance Json Integer" (Aeson.parseJSON v)

instance Json Bool where
  toJson = Aeson.toJSON
  fromJson v = prependFailure "instance Json Bool" (Aeson.parseJSON v)

instance Json Char where
  toJson = Aeson.String . Text.singleton
  fromJson =
    withText
      "instance Json Char"
      ( \txt ->
          case Text.uncons txt of
            (Just (c, rest)) -> if Text.null rest then return c else fail $ "Received a JSON String that has more than one character: " <> Text.unpack txt
            _ -> fail "Received a JSON String that has zero characters"
      )

instance Json ByteString where
  toJson = Aeson.String . encodeByteString
  fromJson = withText "instance Json Bytes" decodeByteString

instance Json Text where
  toJson = Aeson.String
  fromJson = withText "instance Json Text" return

instance Json a => Json (Maybe a) where
  toJson Nothing = toJsonConstructor "Nothing" ()
  toJson (Just x) = toJsonConstructor "Just" x
  fromJson v =
    prependFailure "instance Json (Maybe a) :- Json a" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "Nothing" -> prependFailure "Nothing" $ fromJson @() ctorProduct >> return Nothing
            "Just" -> prependFailure "Just" $ Just <$> fromJson @a ctorProduct
            invalid -> fail $ "Received an invalid constructor name " <> Text.unpack invalid
        )
        v

instance (Json a, Json b) => Json (Either a b) where
  toJson (Left l) = toJsonConstructor "Left" l
  toJson (Right r) = toJsonConstructor "Right" r
  fromJson v =
    prependFailure "instance Json (Either a b) :- Json a, Json b" $
      fromJsonConstructor
        ( \ctorName ctorProduct -> case ctorName of
            "Left" -> prependFailure "Left" $ Left <$> fromJson @a ctorProduct
            "Right" -> prependFailure "Right" $ Right <$> fromJson @b ctorProduct
            invalid -> fail $ "Received an invalid constructor name " <> Text.unpack invalid
        )
        v

instance Json a => Json [a] where
  toJson = Aeson.Array . Vector.fromList . fmap toJson
  fromJson = Aeson.withArray "instance Json (List a) :- Json a" (fmap toList . traverse fromJson)

instance (Json a, Ord a) => Json (Set a) where
  toJson s = toJson (toList s)
  fromJson =
    withArray
      "instance Json (Set a) :- Json a"
      ( \arr -> do
          elems <- prependFailure "Elements" $ fromJson @a `traverse` arr
          let elems' = Set.fromList (toList elems)
          if Vector.length elems == Set.size elems'
            then return elems'
            else fail $ "The JSON Array received has duplicate elements: " <> show arr
      )

instance (Json v) => Json (Map Text v) where
  toJson m = object [k .= toJson v | (k, v) <- Map.toList m]
  fromJson =
    withObject
      "instance Json (Map Text v) :- Json v"
      ( \obj -> do
          let kvs = AesonKeyMap.toList obj
          m <- Map.fromList <$> (\(k, v) -> (AesonKey.toText k,) <$> fromJson @v v) `traverse` kvs
          if List.length kvs == Map.size m
            then return m
            else fail $ "The JSON Object received has duplicate elements: " <> show obj
      )

-- | Helper instances
instance Json () where
  toJson _ = Aeson.Null
  fromJson v =
    if v == Aeson.Null
      then return ()
      else fail $ "[LambdaBuffers.Runtime.Json][Json ()] Expected a null but got " <> show v

instance Json Aeson.Value where
  toJson v = v
  fromJson = return

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = Text.decodeUtf8 . Base64.encode

decodeByteString :: MonadFail m => Text -> m BSS.ByteString
decodeByteString = either (\err -> fail $ "[LambdaBuffers.Runtime.Json.Prelude] Failed decoding from base64 into bytes " <> err) pure . tryDecode
  where
    tryDecode :: Text.Text -> Either String BSS.ByteString
    tryDecode = Base64.decode . Text.encodeUtf8

type Key = Text

(.=) :: Json v => Key -> v -> Aeson.Pair
(.=) k v = (AesonKey.fromText k, toJson v)

(.:) :: forall a. Json a => Aeson.Object -> Key -> Aeson.Parser a
(.:) obj k = Aeson.explicitParseField (fromJson @a) obj (AesonKey.fromText k)

toJsonConstructor :: forall {a}. Json a => Text -> a -> Aeson.Value
toJsonConstructor ctorName ctorProduct = object ["constructor" .= Aeson.String ctorName, "fields" .= toJson ctorProduct]

fromJsonConstructor :: (Text -> Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
fromJsonConstructor f =
  Aeson.withObject
    "[LambdaBuffers.Runtime.Json.Prelude][fromJsonConstructor]"
    ( \obj -> do
        ctorName <- obj .: "constructor" >>= Aeson.withText "[LambdaBuffers.Runtime.Json][constructor]" return
        ctorProduct <- obj .: "fields"
        f ctorName ctorProduct
    )
