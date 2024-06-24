module LambdaBuffers.Codegen.Haskell.Print.Syntax (
  printHsQTyName,
  printCtorName,
  printFieldName,
  printVarName,
  printTyName,
  printMkCtor,
  printModName,
  printHsQValName,
  printHsClassMethodName,
  printHsQClassName,
  printHsValName,
  QTyName,
  QClassName,
  QValName,
  CabalPackageName (..),
  ModuleName (..),
  TyName (..),
  ClassName (..),
  ValueName (..),
  fromLbTyName,
  TyDefKw (..),
  cabalPackageNameToText,
) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, enclose, lparen, rparen, squote)

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

cabalPackageNameToText :: CabalPackageName -> Text
cabalPackageNameToText (MkCabalPackageName cpn) = cpn

printModName :: (PC.ModuleName -> ModuleName) -> PC.ModuleName -> Doc ann
printModName fromLbModuleName mn = let MkModuleName hmn = fromLbModuleName mn in pretty hmn

printHsQTyName :: QTyName -> Doc ann
printHsQTyName (_, MkModuleName hsModName, MkTyName hsTyName) = pretty hsModName <> dot <> pretty hsTyName

printHsQClassName :: QClassName -> Doc ann
printHsQClassName (_, MkModuleName hsModName, MkClassName hsClassName) = pretty hsModName <> dot <> pretty hsClassName

printHsQValName :: QValName -> Doc ann
printHsQValName (_, MkModuleName hsModName, MkValueName hsValName) = case Text.uncons hsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Haskell value name"
  Just (c, _) | Char.isAlpha c -> pretty hsModName <> dot <> pretty hsValName
  _r -> enclose lparen rparen $ pretty hsModName <> dot <> pretty hsValName

printHsValName :: ValueName -> Doc ann
printHsValName (MkValueName hsValName) = case Text.uncons hsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Haskell value name"
  Just (c, _) | Char.isAlpha c -> pretty hsValName
  _r -> enclose lparen rparen $ pretty hsValName

{- | Print the Haskell class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printHsClassMethodName :: QValName -> Doc ann
printHsClassMethodName (_, _, MkValueName hsValName) = pretty hsValName

{- | Translate LambdaBuffer sum constructor names into Haskell sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printCtorName tyN (PC.ConstrName n _) = printTyName tyN <> squote <> pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

{- | Translate LambdaBuffer record field names into Haskell record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = Rec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: PC.TyName -> PC.FieldName -> Maybe (Doc ann)
printFieldName tyN (PC.FieldName n _) = do
  prefix <- case Text.uncons (tyN ^. #name) of
    Nothing -> Nothing
    Just (h, t) -> return $ Text.cons (Char.toLower h) t
  return $ pretty prefix <> squote <> pretty n

printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n
