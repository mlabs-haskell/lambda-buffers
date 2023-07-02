module LambdaBuffers.Codegen.Config (Config (..), cfgOpaques, cfgClasses, moduleNameFromText, qClassNameFromText, qClassNameToText) where

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
import LambdaBuffers.ProtoCompat qualified as PC

-- | `Config` is parametrized over a qualified type and class name for specifying `Opaque` type and class mappings in the target language.
data Config qtn qcn = Config
  { _cfgOpaques :: Map PC.QTyName qtn
  , _cfgClasses :: Map PC.QClassName [qcn]
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'Config

-- | `JsonConfig` is a data type representing the external config format parametrized by qualified type and class names and instantiated per each backend separately.
data JsonConfig qtn qcn = JsonConfig
  { opaquesConfig :: Map Text qtn
  , classesConfig :: Map Text [qcn]
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | `moduleNameToText ["Foo", "Bar"] = "Foo.Bar"`
moduleNameToText :: PC.InfoLess PC.ModuleName -> Text
moduleNameToText = Text.pack . show . (`PC.withInfoLess` PC.prettyModuleName)

{- | `moduleNameFromText "Foo.Bar" = ["Foo", "Bar"]`
 TODO(bladyjoker): Use ModuleNamePart parsers from the Compiler.
-}
moduleNameFromText :: MonadFail m => Text -> m (PC.InfoLess PC.ModuleName)
moduleNameFromText txt = case Text.split (== '.') txt of
  [] -> fail "[LambdaBuffers.Codegen.Config] Got an empty text but wanted a LambdaBuffers module name"
  parts -> return (PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart p def | p <- parts] def)

-- | `qTyNameToText (["Foo", "Bar"], "Baz") = "Foo.Bar.Baz"`
qTyNameToText :: PC.QTyName -> Text
qTyNameToText (mn, tyn) = moduleNameToText mn <> "." <> PC.withInfoLess tyn (view #name)

-- | `qClassNameToText (["Foo", "Bar"], "Baz") = "Foo.Bar.Baz"`
qClassNameToText :: PC.QClassName -> Text
qClassNameToText (mn, cn) = moduleNameToText mn <> "." <> PC.withInfoLess cn (view #name)

qNameFromText :: MonadFail m => Text -> m (PC.InfoLess PC.ModuleName, Text)
qNameFromText txt =
  let xs = Text.split (== '.') txt
   in case List.reverse xs of
        [] -> fail "[LambdaBuffers.Codegen.Config] Got an empty text but wanted a qualified LambdaBuffers name (<module name>.<type name|class name>)"
        [x] -> fail $ "[LambdaBuffers.Codegen.Config] Got a single text " <> show x <> " but wanted a qualified LambdaBuffers name (<module name>.<type name|class name>)"
        (n : mn) ->
          return
            ( PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart p def | p <- List.reverse mn] def
            , n
            )

qTyNameFromText :: MonadFail m => Text -> m PC.QTyName
qTyNameFromText qtyn = qNameFromText qtyn >>= \(mn, n) -> return (mn, PC.mkInfoLess (PC.TyName n def))

-- | `qClassNameFrinText "Foo.Bar.Baz" = (["Foo", "Bar"], "Baz")`
qClassNameFromText :: MonadFail m => Text -> m PC.QClassName
qClassNameFromText qcn = qNameFromText qcn >>= \(mn, n) -> return (mn, PC.mkInfoLess (PC.ClassName n def))

toOpaquesConfig :: Map PC.QTyName qtn -> Map Text qtn
toOpaquesConfig = Map.mapKeys qTyNameToText

fromOpaquesConfig :: MonadFail m => Map Text qtn -> m (Map PC.QTyName qtn)
fromOpaquesConfig opqs = Map.fromList <$> traverse (\(lqtn, qtn) -> (,) <$> qTyNameFromText lqtn <*> pure qtn) (Map.toList opqs)

toClassesConfig :: Map PC.QClassName qcn -> Map Text qcn
toClassesConfig = Map.mapKeys qClassNameToText

fromClassesConfig :: MonadFail m => Map Text qcn -> m (Map PC.QClassName qcn)
fromClassesConfig clss = Map.fromList <$> traverse (\(lqcn, qcn) -> (,) <$> qClassNameFromText lqcn <*> pure qcn) (Map.toList clss)

toJsonConfig :: Config qtn qcn -> JsonConfig qtn qcn
toJsonConfig (Config opqs cls) = JsonConfig (toOpaquesConfig opqs) (toClassesConfig cls)

fromJsonConfig :: MonadFail m => JsonConfig qtn qcn -> m (Config qtn qcn)
fromJsonConfig (JsonConfig opqs cls) = Config <$> fromOpaquesConfig opqs <*> fromClassesConfig cls

instance (ToJSON qtn, ToJSON qcn) => ToJSON (JsonConfig qtn qcn)
instance (FromJSON qtn, FromJSON qcn) => FromJSON (JsonConfig qtn qcn)

instance (ToJSON qtn, ToJSON qcn) => ToJSON (Config qtn qcn) where
  toJSON = toJSON . toJsonConfig

instance (FromJSON qtn, FromJSON qcn) => FromJSON (Config qtn qcn) where
  parseJSON v = parseJSON v >>= fromJsonConfig
