module LambdaBuffers.Runtime.Prelude.Json (class Json, toJson, fromJson, fromJsonString, fromJsonBytes, toJsonString, toJsonBytes) where

import Prelude

import Aeson (Aeson, (.:))
import Aeson as Aeson
import Data.Array (fromFoldable, toUnfoldable)
import Data.Array as Array
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)
import Data.Binary.Base64 as Base64
import Data.Either (Either(..), either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoder (encodeUtf8)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Parser a = Either Aeson.JsonDecodeError a

-- | Class that lbf-prelude.Prelude.Json class maps to
class Json a where
  toJson :: a -> Aeson
  fromJson :: Aeson -> Parser a

-- | lbf-prelude.Prelude.Json instance rule implementations
instance Json BigInt where
  toJson = Aeson.encodeAeson
  fromJson = prependFailure "Prelude.Integer" <<< Aeson.decodeAeson

instance Json Boolean where
  toJson = Aeson.encodeAeson
  fromJson = prependFailure "Prelude.Bool" <<< Aeson.decodeAeson

instance Json Char where
  toJson c = Aeson.fromString <<< String.fromCharArray <<< toUnfoldable $ [ c ]
  fromJson json = prependFailure "Prelude.Char" $ caseJsonString json >>=
    ( \str -> case fromFoldable <<< String.toCharArray $ str of
        [ c ] -> pure c
        _ -> fail $ "Expected a JSON String for length 1 but got " <> show str
    )

instance Json Uint8Array where
  toJson = Aeson.fromString <<< Base64.encode
  fromJson json = prependFailure "Prelude.Bytes" do
    str <- caseJsonString json
    either
      (\err -> fail $ "Expected a base64 encoded JSON String but errored with " <> show err)
      pure
      (Base64.decode str)

instance Json String where
  toJson = Aeson.fromString
  fromJson = prependFailure "Prelude.String" <<< caseJsonString

instance Json a => Json (Maybe a) where
  toJson Nothing = jsonConstructor "Nothing" []
  toJson (Just x) = jsonConstructor "Just" [ toJson x ]
  fromJson = caseJsonConstructor
    "Prelude.Maybe"
    [ Tuple "Nothing"
        ( \ctorFields -> case ctorFields of
            [] -> pure Nothing
            _ -> fail $ "Expected a JSON Array with 0 fields but got " <> show ctorFields
        )
    , Tuple "Just"
        ( \ctorFields -> case ctorFields of
            [ x ] -> Just <$> fromJson x
            _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields
        )
    ]

instance (Json a, Json b) => Json (Either a b) where
  toJson (Left l) = jsonConstructor "Left" [ toJson l ]
  toJson (Right r) = jsonConstructor "Right" [ toJson r ]
  fromJson =
    caseJsonConstructor
      "Prelude.Either"
      [ Tuple "Left" $ \ctorFields -> case ctorFields of
          [ l ] -> Left <$> fromJson l
          _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields

      , Tuple "Right" $ \ctorFields -> case ctorFields of
          [ r ] -> Right <$> fromJson r
          _ -> fail $ "Expected a JSON Array with 1 fields but got " <> show ctorFields

      ]

instance Json a => Json (Array a) where
  toJson = map toJson >>> Aeson.fromArray
  fromJson json = prependFailure "Prelude.List" $ caseJsonArray json >>= traverse fromJson

instance (Json a, Ord a) => Json (Set a) where
  toJson s = toJson (fromFoldable s)
  fromJson json = prependFailure "Prelude.Set" $
    caseJsonArray json >>=
      ( \arr -> do
          elems <- prependFailure "elements" $ fromJson `traverse` arr
          let elems' = Set.fromFoldable elems
          if Array.length elems == Set.size elems' then pure elems'
          else fail $ "Expected the JSON Array to have all unique elements: " <> show arr
      )

instance (Json k, Json v, Ord k) => Json (Map k v) where
  toJson = Map.toUnfoldable >>> map (\(Tuple k v) -> Tuple (toJson k) (toJson v)) >>> jsonMap
  fromJson = caseJsonMap "Prelude.Map" (\(Tuple k v) -> Tuple <$> fromJson k <*> fromJson v)

-- | Helper instances

instance Json Aeson where
  toJson x = x
  fromJson = pure

-- instance Json () where
--   toJson _ = Aeson.Null
--   fromJson v =
--     if v == Aeson.Null
--       then return ()
--       else fail $ "Expected a JSON Null but got " <> show v

-- -- | LamVal Json builtins
-- jsonArray :: [Aeson.Value] -> Aeson.Value
-- jsonArray = Aeson.Array . Vector.fromList

-- caseJsonArray :: String -> ([Aeson.Value] -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
-- caseJsonArray title parseArr = prependFailure "caseJsonArray" . prependFailure title . Aeson.withArray "" ((prependFailure "element " . parseArr) . toList)

-- jsonObject :: [(String, Aeson.Value)] -> Aeson.Value
-- jsonObject kvs = object [k .= v | (k, v) <- kvs]

-- caseJsonObject :: (Aeson.Object -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
-- caseJsonObject parseObj = prependFailure "caseJsonObj" . Aeson.withObject "" parseObj

-- jsonField :: String -> Aeson.Object -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Parser a
-- jsonField name obj parseField = obj .: name >>= parseField

jsonConstructor :: String -> Array Aeson -> Aeson
jsonConstructor = jsonConstructor'

caseJsonConstructor :: forall a. String -> Array (Tuple String (Array Aeson -> Parser a)) -> Aeson -> Parser a
caseJsonConstructor title ctorParsers json = prependFailure "caseJsonConstructor" $ prependFailure title $ caseJsonConstructor' (Map.fromFoldable ctorParsers) json

-- | Utils

jsonConstructor' :: String -> Array Aeson -> Aeson
jsonConstructor' ctorName ctorFields = Aeson.fromObject $ Object.fromFoldable
  [ Tuple "name" (Aeson.fromString ctorName)
  , Tuple "fields" (toJson ctorFields)
  ]

caseJsonConstructor' :: forall a. Map String (Array Aeson -> Parser a) -> Aeson -> Parser a
caseJsonConstructor' ctorParsers json = caseJsonObject json >>=
  ( \obj -> do
      ctorName <- obj .: "name" >>= caseJsonString
      case Map.lookup ctorName ctorParsers of
        Nothing ->
          fail $
            "Expected a JSON String to contain one of constructor names ("
              <> Array.intercalate ", " (fromFoldable $ Map.keys ctorParsers)
              <> "), but got an unknown constructor name "
              <> ctorName
        Just parse -> do
          ctorFields <- obj .: "fields"
          prependFailure
            ("constructor " <> ctorName)
            $ parse ctorFields
  )

fail' ∷ String → Aeson.JsonDecodeError
fail' msg = Aeson.TypeMismatch msg

fail ∷ ∀ a. String → Parser a
fail = Left <<< fail'

prependFailure :: forall a. String -> Parser a -> Parser a
prependFailure prepend (Left (Aeson.TypeMismatch msg)) = Left $ Aeson.TypeMismatch (prepend <> " > " <> msg)
prependFailure _ x = x

caseJsonArray ∷ Aeson → Parser (Array Aeson)
caseJsonArray = Aeson.toArray >>> note (fail' "Expected a JSON Array")

caseJsonString ∷ Aeson → Parser String
caseJsonString = Aeson.toString >>> note (fail' "Expected a JSON String")

caseJsonObject :: Aeson -> Parser (Object Aeson)
caseJsonObject = Aeson.toObject >>> note (fail' "Expected a JSON Object")

jsonMap :: Array (Tuple Aeson Aeson) -> Aeson
jsonMap = map jsonTuple >>> Aeson.fromArray

jsonTuple :: Tuple Aeson Aeson -> Aeson
jsonTuple (Tuple l r) = Aeson.fromArray [ l, r ]

caseJsonTuple :: Aeson -> Parser (Tuple Aeson Aeson)
caseJsonTuple json = do
  jsons <- caseJsonArray json
  case jsons of
    [ l, r ] -> pure $ Tuple l r
    invalid -> fail $ "Expected a JSON Array with 2 elements but got " <> show invalid

caseJsonMap :: forall k v. Ord k => String -> (Tuple Aeson Aeson -> Parser (Tuple k v)) -> Aeson -> Parser (Map k v)
caseJsonMap title parseElem json =
  prependFailure "caseJsonMap" <<< prependFailure title $ caseJsonArray json >>=
    ( \arr -> do
        let parse v = caseJsonTuple v >>= parseElem
        kvs <- prependFailure "element " $ parse `traverse` arr
        let m = Map.fromFoldable kvs
        unless (Array.length arr == Map.size m) (fail $ "Expected a JSON Array with unique elements for map but got " <> show arr)
        pure m
    )

toJsonString :: forall a. Json a => a -> String
toJsonString = toJson >>> Aeson.stringifyAeson

fromJsonString :: forall a. Json a => String -> Parser a
fromJsonString str = Aeson.parseJsonStringToAeson str >>= fromJson

toJsonBytes :: forall a. Json a => a -> Uint8Array
toJsonBytes = toJsonString >>> encodeUtf8

fromJsonBytes :: forall a. Json a => Uint8Array -> Parser a
fromJsonBytes bs =
  case decodeUtf8 bs of
    Left err -> fail $ "Expected UTF-8 bytes but got error " <> show err
    Right str -> fromJsonString str :: Parser a
