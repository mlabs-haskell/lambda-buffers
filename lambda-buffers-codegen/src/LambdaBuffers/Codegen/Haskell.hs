module LambdaBuffers.Codegen.Haskell (printSum) where

import Data.Foldable (Foldable (toList))
import LambdaBuffers.Compiler.ProtoCompat.Types (ConstrName (ConstrName), Constructor (Constructor), Field (Field), FieldName (FieldName), ForeignRef (ForeignRef), LocalRef (LocalRef), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (RecordI, TupleI), Record (Record), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyApp (TyApp), TyName (TyName), TyRef (ForeignI, LocalI), TyVar (TyVar), VarName (VarName))
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, group, lbrace, pipe, rbrace, sep, space, surround, (<+>))

printSum :: Sum -> Doc a
printSum (Sum ctors _) =
  group $
    if null ctors
      then mempty
      else align $ encloseSep mempty mempty (space <> pipe <> space) (printCtor <$> toList ctors)

printCtor :: Constructor -> Doc a
printCtor (Constructor ctorName prod) = align $ group (printCtorName ctorName <> printProd prod)

printCtorName :: ConstrName -> Doc a
printCtorName (ConstrName n _) = pretty n -- TODO(bladyjoker): Constructor name is formed as SumTyName'ConstrName

printProd :: Product -> Doc a
printProd (RecordI rc) = printRec rc
printProd (TupleI tup) = printTup tup

printRec :: Record -> Doc a
printRec (Record fields _) = group $ encloseSep lbrace rbrace (space <> comma <> space) (printField <$> toList fields)

printTup :: Tuple -> Doc a
printTup (Tuple fields _si) = group $ sep (printTy <$> fields)

printField :: Field -> Doc a
printField (Field fn ty) = printFieldName fn <+> colon <> colon <+> printTy ty -- -- TODO(bladyjoker): Field name is formed as recName'fieldName

printTy :: Ty -> Doc a
printTy (TyVarI v) = printTyVar v
printTy (TyRefI r) = printTyRef r
printTy (TyAppI a) = printTyApp a

printTyApp :: TyApp -> Doc a
printTyApp (TyApp f args _) = group $ printTy f <+> align (sep (printTy <$> args))

printTyRef :: TyRef -> Doc a
printTyRef (LocalI (LocalRef tn _)) = group $ printTyName tn
printTyRef (ForeignI (ForeignRef tn mn _)) = group $ printModName mn <> dot <> printTyName tn -- TODO(bladyjoker): Emit Import

printTyVar :: TyVar -> Doc a
printTyVar (TyVar vn _) = printVarName vn

printVarName :: VarName -> Doc a
printVarName (VarName n _) = pretty n

printFieldName :: FieldName -> Doc a
printFieldName (FieldName n _) = pretty n

printTyName :: TyName -> Doc a
printTyName (TyName n _) = pretty n

printModName :: ModuleName -> Doc a
printModName (ModuleName parts _) = group $ concatWith (surround dot) [pretty p | ModuleNamePart p _ <- parts]
