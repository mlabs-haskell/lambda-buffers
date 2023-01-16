{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Frontend.PPrint () where

import Data.List (sort)
import Data.Text qualified as Text
import LambdaBuffers.Frontend.Syntax (ClassName (ClassName), ConstrName (ConstrName), Constructor (Constructor), FieldName (FieldName), Import (Import), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo (SourceInfo), SourcePos (SourcePos), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, Sum), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName))
import Prettyprinter (Doc, Pretty (pretty), align, comma, concatWith, encloseSep, equals, group, hsep, line, lparen, pipe, rparen, space, (<+>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec

intercalate :: Doc a -> [Doc a] -> Doc a
intercalate sep = concatWith (\l r -> l <> sep <> r)

instance (Ord info, Pretty info) => Pretty (Module info) where
  pretty (Module mn imports tyDs _info) =
    let sortedImports = sort imports
     in "module"
          <+> pretty mn
            <> ( if null sortedImports
                  then ""
                  else
                    line
                      <> line
                      <> intercalate line (pretty <$> sortedImports)
               )
            <> ( if null tyDs
                  then ""
                  else
                    line
                      <> line
                      <> intercalate (line <> line) (pretty <$> tyDs)
               )

instance Pretty info => Pretty (Import info) where
  pretty (Import isQ imn maySyms mayAl _info) =
    "import"
      <> (if isQ then space <> "qualified" else "")
      <+> pretty imn
        <> case maySyms of
          Nothing -> ""
          Just syms -> space <> encloseSep lparen rparen comma (pretty <$> syms)
        <> case mayAl of
          Nothing -> ""
          Just al -> space <> "as" <+> pretty al

instance Pretty info => Pretty (TyDef info) where
  pretty (TyDef tn args body@(Sum _ _) _info) = group $ "sum" <+> pretty tn <+> hsep (pretty <$> args) <+> equals <+> pretty body
  pretty (TyDef tn args Opaque _info) = "opaque" <+> pretty tn <> if null args then "" else space <> hsep (pretty <$> args)

instance Pretty info => Pretty (TyBody info) where
  pretty (Sum cs _info) = if null cs then "" else align $ encloseSep "" "" (space <> pipe <> space) (pretty <$> cs)
  pretty Opaque = ""

instance Pretty info => Pretty (TyArg info) where
  pretty (TyArg a _info) = pretty a

instance Pretty info => Pretty (ModuleName info) where
  pretty (ModuleName ps _info) = pretty $ Text.intercalate "." [p | ModuleNamePart p _ <- ps]

instance Pretty info => Pretty (TyName info) where
  pretty (TyName t _info) = pretty t

instance Pretty info => Pretty (VarName info) where
  pretty (VarName t _info) = pretty t

instance Pretty info => Pretty (ConstrName info) where
  pretty (ConstrName t _info) = pretty t

instance Pretty info => Pretty (ClassName info) where
  pretty (ClassName t _info) = pretty t

instance Pretty info => Pretty (FieldName info) where
  pretty (FieldName t _info) = pretty t

instance Pretty info => Pretty (ModuleAlias info) where
  pretty (ModuleAlias mn _info) = pretty mn

instance Pretty info => Pretty (TyRef info) where
  pretty (TyRef mayModAl tn _info) = maybe "" (\al -> pretty al <> ".") mayModAl <> pretty tn

instance Pretty info => Pretty (Ty info) where
  pretty (TyVar vn _info) = pretty vn
  pretty (TyRef' tr _info) = pretty tr
  pretty (TyApp tyF tyAs _info) = group $ encloseSep lparen rparen space (pretty <$> tyF : tyAs)

instance Pretty info => Pretty (Constructor info) where
  pretty (Constructor cn p _info) = align $ group (pretty cn <> pretty p)

instance Pretty info => Pretty (Product info) where
  pretty (Product tys _info) = group $ if null tys then "" else space <> hsep (pretty <$> tys)

instance Pretty SourceInfo where
  pretty (SourceInfo fn pos pos') = pretty fn <> ":" <> "(" <> pretty pos <> ")-(" <> pretty pos' <> ")"

instance Pretty SourcePos where
  pretty (SourcePos r c) = pretty r <> ":" <> pretty c

instance Pretty Parsec.ParseError where
  pretty pe = pretty (Parsec.errorPos pe) <> ":" <> pretty (Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ Parsec.errorMessages pe)

instance Pretty Parsec.SourcePos where
  pretty sp = pretty (Parsec.sourceName sp) <> ":" <> "(" <> pretty (Parsec.sourceLine sp) <> ":" <> pretty (Parsec.sourceColumn sp) <> ")"
