module LambdaBuffers.Codegen.Rust.Print.Syntax (printRsQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printRsQValName, printRsClassMethodName, printRsQClassName, printRsValName, QTyName, QClassName, QValName, CrateName (..), ModuleName (..), TyName (..), ClassName (..), ValueName (..), fromLbModuleName, crateFromLbModuleName, fromLbTyName, fromLbForeignRef, filepathFromModuleName, TyDefKw (..), crateNameToText, printQualifiedCtorName, printTyVar, printTyArg, doubleColon) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), colon, dot, enclose, lparen, rparen)

type QTyName = (CrateName, ModuleName, TyName)
type QClassName = (CrateName, ModuleName, ClassName)
type QValName = (CrateName, ModuleName, ValueName)

newtype CrateName = MkCrateName Text deriving stock (Eq, Ord, Show, Generic)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show, Generic)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show, Generic)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show, Generic)
newtype ValueName = MkValueName Text deriving stock (Eq, Ord, Show, Generic)

data TyDefKw = StructTyDef | EnumTyDef | SynonymTyDef deriving stock (Eq, Ord, Show, Generic)

fromLbTyName :: PC.TyName -> TyName
fromLbTyName tn = MkTyName $ tn ^. #name

fromLbModuleName :: PC.ModuleName -> ModuleName
fromLbModuleName mn = MkModuleName $ Text.intercalate "::" ([Text.toLower $ p ^. #name | p <- mn ^. #parts])

crateFromLbModuleName :: PC.ModuleName -> CrateName
crateFromLbModuleName mn = MkCrateName $ Text.intercalate "-" ("lbf" : [Text.toLower $ p ^. #name | p <- mn ^. #parts])

crateNameToText :: CrateName -> Text
crateNameToText (MkCrateName cpn) = cpn

fromLbForeignRef :: PC.ForeignRef -> QTyName
fromLbForeignRef fr =
  ( crateFromLbModuleName $ fr ^. #moduleName
  , fromLbModuleName $ fr ^. #moduleName
  , fromLbTyName $ fr ^. #tyName
  )

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack (Text.replace "::" "/" (let MkModuleName txt = fromLbModuleName mn in txt)) <> ".rs"

printModName :: PC.ModuleName -> Doc ann
printModName mn = let MkModuleName hmn = fromLbModuleName mn in pretty hmn

printRsQTyName :: QTyName -> Doc ann
printRsQTyName (MkCrateName rsCrateName, MkModuleName rsModName, MkTyName rsTyName) =
  pretty rsCrateName <> doubleColon <> pretty rsModName <> doubleColon <> pretty rsTyName

printRsQClassName :: QClassName -> Doc ann
printRsQClassName (MkCrateName rsCrateName, MkModuleName rsModName, MkClassName rsClassName) =
  pretty rsCrateName <> doubleColon <> pretty rsModName <> doubleColon <> pretty rsClassName

printRsQValName :: QValName -> Doc ann
printRsQValName (_, MkModuleName rsModName, MkValueName rsValName) = case Text.uncons rsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Rust value name"
  Just (c, _) | Char.isAlpha c -> pretty rsModName <> dot <> pretty rsValName
  _ -> enclose lparen rparen $ pretty rsModName <> dot <> pretty rsValName

printRsValName :: ValueName -> Doc ann
printRsValName (MkValueName rsValName) = case Text.uncons rsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Rust value name"
  Just (c, _) | Char.isAlpha c -> pretty rsValName
  _ -> enclose lparen rparen $ pretty rsValName

{- | Print the Rust class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printRsClassMethodName :: QValName -> Doc ann
printRsClassMethodName (_, _, MkValueName rsValName) = pretty rsValName

printCtorName :: PC.ConstrName -> Doc ann
printCtorName (PC.ConstrName n _) = pretty n

printQualifiedCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printQualifiedCtorName tyN (PC.ConstrName n _) = printTyName tyN <> doubleColon <> pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

printFieldName :: PC.FieldName -> Doc ann
printFieldName (PC.FieldName n _) = pretty n

printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty n

printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg (PC.VarName n _) _ _) = pretty $ Text.toTitle n

printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar (PC.VarName n _)) = pretty $ Text.toTitle n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n

doubleColon :: Doc ann
doubleColon = colon <> colon
