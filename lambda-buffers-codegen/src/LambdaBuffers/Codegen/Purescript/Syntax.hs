module LambdaBuffers.Codegen.Purescript.Syntax (
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
) where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

type QTyName = (PackageName, ModuleName, TyName)
type QClassName = (PackageName, ModuleName, ClassName)
type QValName = (PackageName, ModuleName, ValueName)

newtype PackageName = MkPackageName Text deriving stock (Eq, Ord, Show, Generic)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show, Generic)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show, Generic)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show, Generic)
newtype ValueName = MkValueName Text deriving stock (Eq, Ord, Show, Generic)

fromLbTyName :: PC.TyName -> TyName
fromLbTyName tn = MkTyName $ tn ^. #name

fromLbModuleName :: PC.ModuleName -> ModuleName
fromLbModuleName mn = MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts])

pkgFromLbModuleName :: PC.ModuleName -> PackageName
pkgFromLbModuleName mn = MkPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-lb"])

fromLbForeignRef :: PC.ForeignRef -> QTyName
fromLbForeignRef fr =
  ( pkgFromLbModuleName $ fr ^. #moduleName
  , fromLbModuleName $ fr ^. #moduleName
  , fromLbTyName $ fr ^. #tyName
  )

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".hs"
