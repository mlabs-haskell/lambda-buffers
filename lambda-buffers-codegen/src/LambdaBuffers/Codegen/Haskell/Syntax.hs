module LambdaBuffers.Codegen.Haskell.Syntax (QTyName, QClassName, QValName, CabalPackageName (..), ModuleName (..), TyName (..), ClassName (..), ValueName (..), fromLbModuleName, cabalFromLbModuleName, fromLbTyName, fromLbForeignRef, filepathFromModuleName, TyDefKw (..)) where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

type QTyName = (CabalPackageName, ModuleName, TyName)
type QClassName = (CabalPackageName, ModuleName, ClassName)
type QValName = (CabalPackageName, ModuleName, ValueName)

newtype CabalPackageName = MkCabalPackageName Text deriving stock (Eq, Ord, Show, Generic)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show, Generic)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show, Generic)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show, Generic)
newtype ValueName = MkValueName Text deriving stock (Eq, Ord, Show, Generic)

data TyDefKw = DataTyDef | NewtypeTyDef | SynonymTyDef deriving stock (Eq, Ord, Show, Generic)

fromLbTyName :: PC.TyName -> TyName
fromLbTyName tn = MkTyName $ tn ^. #name

fromLbModuleName :: PC.ModuleName -> ModuleName
fromLbModuleName mn = MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts])

-- TODO(bladyjoker): Figure out the Cabal package name syntax.
cabalFromLbModuleName :: PC.ModuleName -> CabalPackageName
cabalFromLbModuleName mn = MkCabalPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-lb"])

fromLbForeignRef :: PC.ForeignRef -> QTyName
fromLbForeignRef fr =
  ( cabalFromLbModuleName $ fr ^. #moduleName
  , fromLbModuleName $ fr ^. #moduleName
  , fromLbTyName $ fr ^. #tyName
  )

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".hs"
