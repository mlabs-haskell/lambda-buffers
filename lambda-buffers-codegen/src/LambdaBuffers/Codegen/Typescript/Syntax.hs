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
) where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.ProtoCompat.Types qualified as PC

type QTyName = (PackageName, ModuleName, TyName)
type QClassName = (PackageName, ModuleName, ClassName)
type QValName = (Maybe (PackageName, ModuleName), ValueName)

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
