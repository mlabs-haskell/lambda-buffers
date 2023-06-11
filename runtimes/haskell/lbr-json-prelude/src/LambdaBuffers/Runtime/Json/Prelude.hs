module LambdaBuffers.Runtime.Json.Prelude (
  Json (toJson, fromJson),
  toJsonBytes,
  fromJsonBytes,
  (.:),
  (.=),
  jsonConstructor,
  caseJsonConstructor,
  jsonArray,
  caseJsonArray,
  jsonMap,
  caseJsonMap,
  caseJsonObject,
  jsonField,
)
where

import Control.Monad (unless)
import Data.Aeson (object, withArray, withText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEnc
import Data.Aeson.Key qualified as AesonKey
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
  toJson Nothing = jsonConstructor "Nothing" []
  toJson (Just x) = jsonConstructor "Just" [toJson x]
  fromJson =
    caseJsonConstructor
      "instance Json (Maybe a) :- Json a"
      [
        ( "Nothing"
        , \ctorFields -> case ctorFields of
            [] -> return Nothing
            _ -> fail $ "Expected a JSON Array with 0 fields but got " <> show ctorFields
        )
      ,
        ( "Just"
        , \ctorFields -> case ctorFields of
            [x] -> Just <$> fromJson @a x
            _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields
        )
      ]

instance (Json a, Json b) => Json (Either a b) where
  toJson (Left l) = jsonConstructor "Left" [toJson l]
  toJson (Right r) = jsonConstructor "Right" [toJson r]
  fromJson =
    caseJsonConstructor
      "instance Json (Either a b) :- Json a, Json b"
      [
        ( "Left"
        , \ctorFields -> case ctorFields of
            [l] -> Left <$> fromJson @a l
            _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields
        )
      ,
        ( "Right"
        , \ctorFields -> case ctorFields of
            [r] -> Right <$> fromJson @b r
            _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields
        )
      ]

instance Json a => Json [a] where
  toJson = jsonArray . fmap toJson
  fromJson = caseJsonArray "instance Json (List a) :- Json a" (\xs -> fromJson @a `traverse` xs)

instance (Json a, Ord a) => Json (Set a) where
  toJson s = toJson (toList s)
  fromJson =
    withArray
      "instance Json (Set a) :- Json a"
      ( \arr -> do
          elems <- prependFailure "Trying Set elements" $ fromJson @a `traverse` arr
          let elems' = Set.fromList (toList elems)
          if Vector.length elems == Set.size elems'
            then return elems'
            else fail $ "Expected the JSON Array to have all unique elements: " <> show arr
      )

instance (Json k, Json v, Ord k) => Json (Map k v) where
  toJson m = jsonMap [(toJson k, toJson v) | (k, v) <- Map.toList m]
  fromJson =
    caseJsonMap "instance Json (Map k v) :- Json k, Json v" (\(k, v) -> (,) <$> fromJson @k k <*> fromJson @v v)

-- | Helper instances
instance Json () where
  toJson _ = Aeson.Null
  fromJson v =
    if v == Aeson.Null
      then return ()
      else fail $ "Expected a JSON Null but got " <> show v

instance Json Aeson.Value where
  toJson v = v
  fromJson = return

instance (Json a, Json b) => Json (a, b) where
  toJson (x, y) = Aeson.Array . Vector.fromList $ [toJson x, toJson y]
  fromJson v =
    withArray
      "Trying a tuple"
      ( \arr -> case Vector.uncons arr of
          Nothing -> fail "Expected a JSON Array of 2 elements but got 0"
          Just (x, rest) -> case Vector.uncons rest of
            Nothing -> fail "Expected a JSON Array of 2 elements but got 1"
            Just (y, rest') -> do
              unless (Vector.null rest') $ fail $ "Expected a JSON Array of 2 elements but got " <> show v
              x' <- fromJson x
              y' <- fromJson y
              return (x', y')
      )
      v

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = Text.decodeUtf8 . Base64.encode

decodeByteString :: MonadFail m => Text -> m BSS.ByteString
decodeByteString = either (\err -> fail $ "Failed decoding from base64 into bytes " <> err) pure . tryDecode
  where
    tryDecode :: Text.Text -> Either String BSS.ByteString
    tryDecode = Base64.decode . Text.encodeUtf8

type Key = Text

(.=) :: Json v => Key -> v -> Aeson.Pair
(.=) k v = (AesonKey.fromText k, toJson v)

(.:) :: forall a. Json a => Aeson.Object -> Key -> Aeson.Parser a
(.:) obj k = Aeson.explicitParseField (fromJson @a) obj (AesonKey.fromText k)

-- | LamVal Json builtins
jsonArray :: [Aeson.Value] -> Aeson.Value
jsonArray = Aeson.Array . Vector.fromList

caseJsonArray :: String -> ([Aeson.Value] -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
caseJsonArray title parseArr = Aeson.withArray ("caseJsonArray " <> title) ((prependFailure "array element " . parseArr) . toList)

jsonMap :: [(Aeson.Value, Aeson.Value)] -> Aeson.Value
jsonMap = jsonArray . fmap toJson

caseJsonMap :: Ord k => String -> ((Aeson.Value, Aeson.Value) -> Aeson.Parser (k, v)) -> Aeson.Value -> Aeson.Parser (Map k v)
caseJsonMap title parseElem =
  Aeson.withArray
    ("caseJsonMap " <> title)
    ( \arr -> do
        let parse v = fromJson @(Aeson.Value, Aeson.Value) v >>= parseElem
        kvs <- Vector.toList <$> (prependFailure "element " . parse) `traverse` arr
        let m = Map.fromList kvs
        unless (Vector.length arr == Map.size m) (fail $ "Expected a JSON Array with unique elements for map but got " <> show arr)
        return m
    )

caseJsonObject :: (Aeson.Object -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
caseJsonObject = Aeson.withObject ""

jsonField :: Text -> Aeson.Object -> Aeson.Parser Aeson.Value
jsonField name obj = obj .: name

jsonConstructor :: Text -> [Aeson.Value] -> Aeson.Value
jsonConstructor ctorName ctorProduct = object ["constructor" .= Aeson.String ctorName, "fields" .= toJson ctorProduct]

caseJsonConstructor :: String -> [(Text, [Aeson.Value] -> Aeson.Parser a)] -> Aeson.Value -> Aeson.Parser a
caseJsonConstructor title = caseJsonConstructor' title . Map.fromList

caseJsonConstructor' :: String -> Map Text ([Aeson.Value] -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
caseJsonConstructor' title ctorParsers =
  Aeson.withObject
    ("caseJsonConstructor " <> title)
    ( \obj -> do
        ctorName <- obj .: "constructor" >>= Aeson.withText "constructor name" return
        case Map.lookup ctorName ctorParsers of
          Nothing ->
            fail $
              "Expected a JSON String to contain one of constructor names ("
                <> List.intercalate ", " (Text.unpack <$> Map.keys ctorParsers)
                <> "), but got an unknown constructor name "
                <> Text.unpack ctorName
          Just parse -> do
            ctorFields <- obj .: "fields"
            prependFailure
              ("constructor fields " <> Text.unpack ctorName)
              $ parse ctorFields
    )
