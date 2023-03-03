{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Haskell.Config (Config (..), opaques, classes) where

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
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

data Config = MkConfig
  { _opaques :: Map PC.QTyName H.QTyName
  , _classes :: Map PC.QClassName (H.QClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig

newtype JsonConfig = MkJsonConfig
  { opaquesConfig :: Map Text H.QTyName
  }
  deriving stock (Eq, Ord, Show, Generic)

lbQTyNameToText :: PC.QTyName -> Text
lbQTyNameToText (mn, tyn) = Text.intercalate "." $ PC.withInfoLess mn (\mn' -> mn' ^. #parts ^.. traverse . #name) <> [PC.withInfoLess tyn (view #name)]

lbQTyNameFromText :: MonadFail m => Text -> m PC.QTyName
lbQTyNameFromText qtyn =
  let xs = Text.split (== '.') qtyn
   in case List.reverse xs of
        [] -> fail "Got an empty text but wanted a qualified LambdaBuffers type name (<module name>.<type name>)"
        [x] -> fail $ "Got a single text " <> show x <> " but wanted a qualified LambdaBuffers type name (<module name>.<type name>)"
        (tyn : mn) ->
          return
            ( PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart p def | p <- List.reverse mn] def
            , PC.mkInfoLess $ PC.TyName tyn def
            )

toOpaquesConfig :: Map PC.QTyName H.QTyName -> Map Text H.QTyName
toOpaquesConfig = Map.mapKeys lbQTyNameToText

fromOpaquesConfig :: MonadFail m => Map Text H.QTyName -> m (Map PC.QTyName H.QTyName)
fromOpaquesConfig opqs = Map.fromList <$> traverse (\(ltyn, htyn) -> (,) <$> lbQTyNameFromText ltyn <*> pure htyn) (Map.toList opqs)

toJsonConfig :: Config -> JsonConfig
toJsonConfig (MkConfig opqs _) = MkJsonConfig (toOpaquesConfig opqs)

fromJsonConfig :: MonadFail m => JsonConfig -> m Config
fromJsonConfig (MkJsonConfig opqs) = MkConfig <$> fromOpaquesConfig opqs <*> pure mempty

instance ToJSON H.TyName
instance FromJSON H.TyName

instance ToJSON H.ModuleName
instance FromJSON H.ModuleName

instance ToJSON H.CabalPackageName
instance FromJSON H.CabalPackageName

instance ToJSON JsonConfig
instance FromJSON JsonConfig

instance ToJSON Config where
  toJSON = toJSON . toJsonConfig

instance FromJSON Config where
  parseJSON v = parseJSON v >>= fromJsonConfig
