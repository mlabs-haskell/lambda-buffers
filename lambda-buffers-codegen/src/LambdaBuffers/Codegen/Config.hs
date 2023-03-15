module LambdaBuffers.Codegen.Config (Config (..), opaques, classes) where

import Control.Lens (makeLenses, view, (^.), (^..))
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Default (Default (def))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

data Config o c = MkConfig
  { _opaques :: Map PC.QTyName o
  , _classes :: Map PC.QClassName c
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig

newtype JsonConfig o = MkJsonConfig
  { opaquesConfig :: Map Text o
  }
  deriving stock (Eq, Ord, Show, Generic)

qTyNameToText :: PC.QTyName -> Text
qTyNameToText (mn, tyn) = Text.intercalate "." $ PC.withInfoLess mn (\mn' -> mn' ^. #parts ^.. traverse . #name) <> [PC.withInfoLess tyn (view #name)]

qTyNameFromText :: MonadFail m => Text -> m PC.QTyName
qTyNameFromText qtyn =
  let xs = Text.split (== '.') qtyn
   in case List.reverse xs of
        [] -> fail "Got an empty text but wanted a qualified LambdaBuffers type name (<module name>.<type name>)"
        [x] -> fail $ "Got a single text " <> show x <> " but wanted a qualified LambdaBuffers type name (<module name>.<type name>)"
        (tyn : mn) ->
          return
            ( PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart p def | p <- List.reverse mn] def
            , PC.mkInfoLess $ PC.TyName tyn def
            )

toOpaquesConfig :: Map PC.QTyName o -> Map Text o
toOpaquesConfig = Map.mapKeys qTyNameToText

fromOpaquesConfig :: MonadFail m => Map Text o -> m (Map PC.QTyName o)
fromOpaquesConfig opqs = Map.fromList <$> traverse (\(ltyn, htyn) -> (,) <$> qTyNameFromText ltyn <*> pure htyn) (Map.toList opqs)

toJsonConfig :: Config o c -> JsonConfig o
toJsonConfig (MkConfig opqs _) = MkJsonConfig (toOpaquesConfig opqs)

fromJsonConfig :: MonadFail m => JsonConfig o -> m (Config o c)
fromJsonConfig (MkJsonConfig opqs) = MkConfig <$> fromOpaquesConfig opqs <*> pure mempty

instance ToJSON o => ToJSON (JsonConfig o)
instance FromJSON o => FromJSON (JsonConfig o)

instance ToJSON o => ToJSON (Config o c) where
  toJSON = toJSON . toJsonConfig

instance FromJSON o => FromJSON (Config o c) where
  parseJSON v = parseJSON v >>= fromJsonConfig
