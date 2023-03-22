module LambdaBuffers.Codegen.Purescript.Print.Names (printPursQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printModName', printPursQValName, printPursClassMethodName, printPursQClassName, printPursValName) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Purescript.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, enclose, lparen, rparen, squote)

printModName' :: PC.InfoLess PC.ModuleName -> Doc ann
printModName' = (`PC.withInfoLess` printModName)

printModName :: PC.ModuleName -> Doc ann
printModName mn = let H.MkModuleName pmn = H.fromLbModuleName mn in pretty pmn

printPursQTyName :: H.QTyName -> Doc ann
printPursQTyName (_, H.MkModuleName pursModName, H.MkTyName pursTyName) = pretty pursModName <> dot <> pretty pursTyName

printPursQClassName :: H.QClassName -> Doc ann
printPursQClassName (_, H.MkModuleName pursModName, H.MkClassName pursClassName) = pretty pursModName <> dot <> pretty pursClassName

printPursQValName :: H.QValName -> Doc ann
printPursQValName (_, H.MkModuleName pursModName, H.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Purescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursModName <> dot <> pretty pursValName
  _ -> enclose lparen rparen $ pretty pursModName <> dot <> pretty pursValName

printPursValName :: H.ValueName -> Doc ann
printPursValName (H.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Purescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> enclose lparen rparen $ pretty pursValName

{- | Print the Purescript class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printPursClassMethodName :: H.QValName -> Doc ann
printPursClassMethodName (_, _, H.MkValueName pursValName) = pretty pursValName

{- | Translate LambdaBuffer sum constructor names into Purescript sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printCtorName tyN (PC.ConstrName n _) = printTyName tyN <> squote <> pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor tyN = "Mk" <> printTyName tyN

{- | Translate LambdaBuffer record field names into Purescript record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
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
