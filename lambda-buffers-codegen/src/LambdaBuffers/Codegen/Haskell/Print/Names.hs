module LambdaBuffers.Codegen.Haskell.Print.Names (printHsQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printModName', printHsQValName, printHsClassMethodName, printHsQClassName, printHsValName) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, enclose, lparen, rparen, squote)

printModName' :: PC.InfoLess PC.ModuleName -> Doc ann
printModName' = (`PC.withInfoLess` printModName)

printModName :: PC.ModuleName -> Doc ann
printModName mn = let H.MkModuleName hmn = H.fromLbModuleName mn in pretty hmn

printHsQTyName :: H.QTyName -> Doc ann
printHsQTyName (_, H.MkModuleName hsModName, H.MkTyName hsTyName) = pretty hsModName <> dot <> pretty hsTyName

printHsQClassName :: H.QClassName -> Doc ann
printHsQClassName (_, H.MkModuleName hsModName, H.MkClassName hsClassName) = pretty hsModName <> dot <> pretty hsClassName

printHsQValName :: H.QValName -> Doc ann
printHsQValName (_, H.MkModuleName hsModName, H.MkValueName hsValName) = case Text.uncons hsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Haskell value name"
  Just (c, _) | Char.isAlpha c -> pretty hsModName <> dot <> pretty hsValName
  _ -> enclose lparen rparen $ pretty hsModName <> dot <> pretty hsValName

printHsValName :: H.ValueName -> Doc ann
printHsValName (H.MkValueName hsValName) = case Text.uncons hsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Haskell value name"
  Just (c, _) | Char.isAlpha c -> pretty hsValName
  _ -> enclose lparen rparen $ pretty hsValName

{- | Print the Haskell class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printHsClassMethodName :: H.QValName -> Doc ann
printHsClassMethodName (_, _, H.MkValueName hsValName) = pretty hsValName

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
