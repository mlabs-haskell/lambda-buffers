module LambdaBuffers.Codegen.Typescript.Print.Names (printTsQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printModName', printTsQValName, printTsClassMethodName, printTsQClassName, printTsValName, printTsUnqualifiedQClassName, printLowerTsUnqualifiedQClassName, printTsQTyNameKey) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, enclose, lparen, rparen)

printModName' :: PC.InfoLess PC.ModuleName -> Doc ann
printModName' = (`PC.withInfoLess` printModName)

printModName :: PC.ModuleName -> Doc ann
printModName mn = let Ts.MkModuleName pmn = Ts.fromLbModuleName mn in pretty pmn

printTsQTyName :: Ts.QTyName -> Doc ann
printTsQTyName (_, Ts.MkModuleName pursModName, Ts.MkTyName pursTyName) = pretty pursModName <> dot <> pretty pursTyName

-- | Prints the unique symbol associated with each type
printTsQTyNameKey :: Ts.QTyName -> Doc ann
printTsQTyNameKey (_, Ts.MkModuleName pursModName, Ts.MkTyName pursTyName) = pretty pursModName <> dot <> pretty pursTyName

printTsQClassName :: Ts.QClassName -> Doc ann
printTsQClassName (_, Ts.MkModuleName pursModName, Ts.MkClassName pursClassName) = pretty pursModName <> dot <> pretty pursClassName

printTsUnqualifiedQClassName :: Ts.QClassName -> Doc ann
printTsUnqualifiedQClassName (_, Ts.MkModuleName _pursModName, Ts.MkClassName pursClassName) = pretty pursClassName

printLowerTsUnqualifiedQClassName :: Ts.QClassName -> Doc ann
printLowerTsUnqualifiedQClassName (_, Ts.MkModuleName _pursModName, Ts.MkClassName pursClassName) =
  pretty $ case Text.uncons pursClassName of
    Nothing -> "TODO(jaredponn): Got an empty Typescript class name"
    Just (c, cs) -> Text.cons (Char.toLower c) cs

printTsQValName :: Ts.QValName -> Doc ann
printTsQValName (Just (_, Ts.MkModuleName pursModName), Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursModName <> dot <> pretty pursValName
  _ -> pretty pursModName <> dot <> pretty pursValName
-- _ -> pretty pursModName <> dot <> enclose lparen rparen (pretty pursValName)
printTsQValName (Nothing, Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  -- _ -> enclose lparen rparen (pretty pursValName)
  _ -> pretty pursValName

printTsValName :: Ts.ValueName -> Doc ann
printTsValName (Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(bladyjoker): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> enclose lparen rparen $ pretty pursValName

{- | Print the Typescript class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printTsClassMethodName :: Ts.QValName -> Doc ann
printTsClassMethodName (_, Ts.MkValueName pursValName) = pretty pursValName

{- | Translate LambdaBuffer sum constructor names into Typescript sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Foo Int | Bar String

 [only note the type constructors]
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printCtorName _tyN (PC.ConstrName n _) = pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

{- | Translate LambdaBuffer record field names into Typescript record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: PC.TyName -> PC.FieldName -> Maybe (Doc ann)
printFieldName _tyN (PC.FieldName n _) = Just (pretty n)

{- | Prints the variable name with `$` prepended since variable names (e.g.
 `number` is a valid variable name in .lbf files, but not in Typescript) may
 overlap builtin Typescript types.
 `myTyVar`
 translates to
 `$myTyVar`
-}
printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty '$' <> pretty n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n
