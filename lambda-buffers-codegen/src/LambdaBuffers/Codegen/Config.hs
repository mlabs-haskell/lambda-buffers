module LambdaBuffers.Codegen.Config (Config (..), opaques, classes) where

import Control.Lens (makeLenses, view)
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
import LambdaBuffers.Compiler.ProtoCompat.Utils qualified as PC

data Config o c = MkConfig
  { _opaques :: Map PC.QTyName o
  , _classes :: Map PC.QClassName c
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig

data JsonConfig o c = MkJsonConfig
  { opaquesConfig :: Map Text o
  , classesConfig :: Map Text c
  }
  deriving stock (Eq, Ord, Show, Generic)

moduleNameToText :: PC.InfoLess PC.ModuleName -> Text
moduleNameToText = Text.pack . show . (`PC.withInfoLess` PC.prettyModuleName)

qTyNameToText :: PC.QTyName -> Text
qTyNameToText (mn, tyn) = moduleNameToText mn <> "." <> PC.withInfoLess tyn (view #name)

qClassNameToText :: PC.QClassName -> Text
qClassNameToText (mn, cn) = moduleNameToText mn <> "." <> PC.withInfoLess cn (view #name)

qNameFromText :: MonadFail m => Text -> m (PC.InfoLess PC.ModuleName, Text)
qNameFromText qn =
  let xs = Text.split (== '.') qn
   in case List.reverse xs of
        [] -> fail "Got an empty text but wanted a qualified LambdaBuffers name (<module name>.<type name|class name>)"
        [x] -> fail $ "Got a single text " <> show x <> " but wanted a qualified LambdaBuffers name (<module name>.<type name|class name>)"
        (n : mn) ->
          return
            ( PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart p def | p <- List.reverse mn] def
            , n
            )

qTyNameFromText :: MonadFail m => Text -> m PC.QTyName
qTyNameFromText qtyn = qNameFromText qtyn >>= \(mn, n) -> return (mn, PC.mkInfoLess (PC.TyName n def))

qClassNameFromText :: MonadFail m => Text -> m PC.QClassName
qClassNameFromText qcn = qNameFromText qcn >>= \(mn, n) -> return (mn, PC.mkInfoLess (PC.ClassName n def))

toOpaquesConfig :: Map PC.QTyName o -> Map Text o
toOpaquesConfig = Map.mapKeys qTyNameToText

fromOpaquesConfig :: MonadFail m => Map Text o -> m (Map PC.QTyName o)
fromOpaquesConfig opqs = Map.fromList <$> traverse (\(ltyn, htyn) -> (,) <$> qTyNameFromText ltyn <*> pure htyn) (Map.toList opqs)

toClassesConfig :: Map PC.QClassName o -> Map Text o
toClassesConfig = Map.mapKeys qClassNameToText

fromClassesConfig :: MonadFail m => Map Text o -> m (Map PC.QClassName o)
fromClassesConfig opqs = Map.fromList <$> traverse (\(ltyn, htyn) -> (,) <$> qClassNameFromText ltyn <*> pure htyn) (Map.toList opqs)

toJsonConfig :: Config o c -> JsonConfig o c
toJsonConfig (MkConfig opqs cls) = MkJsonConfig (toOpaquesConfig opqs) (toClassesConfig cls)

fromJsonConfig :: MonadFail m => JsonConfig o c -> m (Config o c)
fromJsonConfig (MkJsonConfig opqs cls) = MkConfig <$> fromOpaquesConfig opqs <*> fromClassesConfig cls

instance (ToJSON o, ToJSON c) => ToJSON (JsonConfig o c)
instance (FromJSON o, FromJSON c) => FromJSON (JsonConfig o c)

instance (ToJSON o, ToJSON c) => ToJSON (Config o c) where
  toJSON = toJSON . toJsonConfig

instance (FromJSON o, FromJSON c) => FromJSON (Config o c) where
  parseJSON v = parseJSON v >>= fromJsonConfig
