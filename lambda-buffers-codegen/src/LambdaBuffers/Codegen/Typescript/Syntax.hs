module LambdaBuffers.Codegen.Typescript.Syntax (
  QTyName,
  QClassName,
  QValName,
  PackageName (..),
  ModuleName (..),
  TyName (..),
  ClassName (..),
  ValueName (..),
  fromLbModuleName,
  pkgFromLbModuleName,
  fromLbTyName,
  fromLbForeignRef,
  filepathFromModuleName,
  normalValName,
  primValName,
  className,
  pkgNameToText,
  PkgMap,
) where

import Control.Lens ((^.))
import Control.Monad qualified as Monad
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.ProtoCompat.Types qualified as PC

type QTyName = (PackageName, ModuleName, TyName)
type QClassName = (PackageName, ModuleName, ClassName)
type QValName = (Maybe (PackageName, ModuleName), ValueName)

type PkgMap = Map ModuleName PackageName

mkPkgMap :: MonadFail m => Map Text [Text] -> m PkgMap
mkPkgMap = Monad.foldM go Map.toList Map.empty
  where
    go :: MonadFail m => PkgMap -> (Text, [Text]) -> m PkgMap
    go acc (packageName, modules) = do
      pcModules <- traverse Config.moduleNameFromText modules
      let modsToPkg = Map.fromList $ map (,MkPackageName packageName) pcModules
          intersection = Map.intersection acc modsToPkg
      -- TODO(jaredponn): probably should report all errors instead of just the
      -- first..
      unless (Map.isEmpty intersection) $ fail $ "Module name does not uniquely map to a package: " ++ show intersection
      return $ modsToPkg `Map.union` acc

primValName :: Text -> QValName
primValName vn = (Nothing, MkValueName vn)

normalValName :: Text -> Text -> Text -> QValName
normalValName pkg mn vn = (Just (MkPackageName pkg, MkModuleName mn), MkValueName vn)

className :: Text -> Text -> Text -> QClassName
className pkg mn cn = (MkPackageName pkg, MkModuleName mn, MkClassName cn)

newtype PackageName = MkPackageName Text deriving stock (Eq, Ord, Show, Generic)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show, Generic)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show, Generic)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show, Generic)
newtype ValueName = MkValueName Text deriving stock (Eq, Ord, Show, Generic)

fromLbTyName :: PC.TyName -> TyName
fromLbTyName tn = MkTyName $ tn ^. #name

-- Annoyingly, these must be valid Javascript identifiers which cannot have a
-- @.@ in them (as is the usual Haskell syntax for importing qualified
-- modules), so we instead replace the @.@ with a @$@.
fromLbModuleName :: PC.ModuleName -> ModuleName
fromLbModuleName mn = MkModuleName $ Text.intercalate "$" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts])

pkgNameToText :: PackageName -> Text
pkgNameToText (MkPackageName pkg) = pkg

pkgFromLbModuleName :: PC.ModuleName -> PackageName
-- pkgFromLbModuleName mn = MkPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-lb"])
pkgFromLbModuleName mn = MkPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["lb"])

fromLbForeignRef :: PC.ForeignRef -> QTyName
fromLbForeignRef fr =
  ( pkgFromLbModuleName $ fr ^. #moduleName
  , fromLbModuleName $ fr ^. #moduleName
  , fromLbTyName $ fr ^. #tyName
  )

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".mts"
