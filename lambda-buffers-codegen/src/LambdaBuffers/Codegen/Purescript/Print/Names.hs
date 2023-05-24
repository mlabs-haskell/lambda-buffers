module LambdaBuffers.Codegen.Purescript.Print.Names (printPursQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printModName', printPursQValName, printPursClassMethodName, printPursQClassName, printPursValName) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, enclose, lparen, rparen, squote)

printModName' :: PC.InfoLess PC.ModuleName -> Doc ann
printModName' = (`PC.withInfoLess` printModName)

printModName :: PC.ModuleName -> Doc ann
printModName mn = let Purs.MkModuleName pmn = Purs.fromLbModuleName mn in pretty pmn

printPursQTyName :: Purs.QTyName -> Doc ann
printPursQTyName (_, Purs.MkModuleName pursModName, Purs.MkTyName pursTyName) = pretty pursModName <> dot <> pretty pursTyName

printPursQClassName :: Purs.QClassName -> Doc ann
printPursQClassName (_, Purs.MkModuleName pursModName, Purs.MkClassName pursClassName) = pretty pursModName <> dot <> pretty pursClassName

printPursQValName :: Purs.QValName -> Doc ann
printPursQValName (Just (_, Purs.MkModuleName pursModName), Purs.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Purescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursModName <> dot <> pretty pursValName
  _ -> pretty pursModName <> dot <> enclose lparen rparen (pretty pursValName)
printPursQValName (Nothing, Purs.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Purescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> enclose lparen rparen (pretty pursValName)

printPursValName :: Purs.ValueName -> Doc ann
printPursValName (Purs.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Purescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> enclose lparen rparen $ pretty pursValName

{- | Print the Purescript class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printPursClassMethodName :: Purs.QValName -> Doc ann
printPursClassMethodName (_, Purs.MkValueName pursValName) = pretty pursValName

{- | Translate LambdaBuffer sum constructor names into Purescript sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printCtorName tyN (PC.ConstrName n _) = printTyName tyN <> squote <> pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

{- | Translate LambdaBuffer record field names into Purescript record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: PC.TyName -> PC.FieldName -> Maybe (Doc ann)
printFieldName _tyN (PC.FieldName n _) = return $ pretty n

printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n
