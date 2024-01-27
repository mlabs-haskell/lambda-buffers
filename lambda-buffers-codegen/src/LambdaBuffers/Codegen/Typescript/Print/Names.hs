module LambdaBuffers.Codegen.Typescript.Print.Names (printTsQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printModName', printTsQValName, printTsQClassName, printTsValName, printTsUnqualifiedQClassName, printLowerTsUnqualifiedQClassName, printTsQTyNameKey) where

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
printTsQTyName (_, Ts.MkModuleName tsModName, Ts.MkTyName pursTyName) = pretty tsModName <> dot <> pretty pursTyName

{- | Prints the unique symbol associated with each type
Why is this here?
TS doesn't have type classes, so we at runtime resolve which instance should
be used. This means that at runtime, we need some sort of runtime evidence
associated with each type to resolve the instance dictionaries.
Hence, the existence of this function. Note that this runtime evidence has
the same name as the type.
-}
printTsQTyNameKey :: Ts.QTyName -> Doc ann
printTsQTyNameKey (_, Ts.MkModuleName tsModName, Ts.MkTyName pursTyName) = pretty tsModName <> dot <> pretty pursTyName

printTsQClassName :: Ts.QClassName -> Doc ann
printTsQClassName (_, Ts.MkModuleName tsModName, Ts.MkClassName pursClassName) = pretty tsModName <> dot <> pretty pursClassName

printTsUnqualifiedQClassName :: Ts.QClassName -> Doc ann
printTsUnqualifiedQClassName (_, Ts.MkModuleName _tsModName, Ts.MkClassName pursClassName) = pretty pursClassName

printLowerTsUnqualifiedQClassName :: Ts.QClassName -> Doc ann
printLowerTsUnqualifiedQClassName (_, Ts.MkModuleName _tsModName, Ts.MkClassName pursClassName) =
  pretty $ case Text.uncons pursClassName of
    Nothing -> "TODO(jaredponn): Got an empty Typescript class name"
    Just (c, cs) -> Text.cons (Char.toLower c) cs

printTsQValName :: Ts.QValName -> Doc ann
printTsQValName (Just (_, Ts.MkModuleName tsModName), Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(jaredponn): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty tsModName <> dot <> pretty pursValName
  _ -> pretty tsModName <> dot <> pretty pursValName
printTsQValName (Nothing, Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(jaredponn): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> pretty pursValName

printTsValName :: Ts.ValueName -> Doc ann
printTsValName (Ts.MkValueName pursValName) = case Text.uncons pursValName of
  Nothing -> "TODO(jaredponn): Got an empty Typescript value name"
  Just (c, _) | Char.isAlpha c -> pretty pursValName
  _ -> enclose lparen rparen $ pretty pursValName

{- | Translate LambdaBuffer sum constructor names into Typescript sum constructor names.


     Note(jaredponn): This function makes more sense in other languages like
     Haskell where value constructors must have distinct names (w.r.t to other
     constructors in the same module), so it makes sense to print things like
     @TypeName'ConstrName@, but in TS, we just print @ConstrName@
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printCtorName _tyN (PC.ConstrName n _) = pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

{- | Translate LambdaBuffer record field names into Typescript record field
     names.

     Note(jaredponn): this function makes more sense in other languages like Haskell, since
     records must have distinct names. But in TS, we don't care -- records can
     have overlapping names.
-}
printFieldName :: PC.TyName -> PC.FieldName -> Maybe (Doc ann)
printFieldName _tyN (PC.FieldName n _) = Just (pretty n)

{- | Prints the variable name with `$` prepended since variable names may
     overlap builtin Typescript types.
     For example:
        - @number@ is a valid variable name in .lbf files
        - but @number@ cannot be a variable name in TS (as @number@ is a builtin type)


     An example translation is as follows.
     @
        myTyVar
     @
     translates to
     @
        $myTyVar
     @
-}
printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty '$' <> pretty n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n
