module LambdaBuffers.Codegen.Haskell.Syntax (QTyName, QClassName, CabalPackageName (..), ModuleName (..), TyName (..), ClassName (..), FunctionName (..), fromLbModuleName, cabalFromLbModuleName, fromLbTyName, fromLbForeignRef) where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

type QTyName = (CabalPackageName, ModuleName, TyName)
type QClassName = (CabalPackageName, ModuleName, ClassName)

newtype CabalPackageName = MkCabalPackageName Text deriving stock (Eq, Ord, Show)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show)
newtype FunctionName = MkFunctionName Text deriving stock (Eq, Ord, Show)

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
