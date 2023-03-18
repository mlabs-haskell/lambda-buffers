{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : LambdaBuffers.Frontend.PPrint
Description : Pretty printing implementation of the LambdaBuffers.Frontend.Syntax types used for formatting
-}
module LambdaBuffers.Frontend.PPrint () where

import Data.List (sort)
import Data.Text qualified as Text
import LambdaBuffers.Frontend.Syntax (ClassName (ClassName), ConstrName (ConstrName), Constructor (Constructor), Field (Field), FieldName (FieldName), Import (Import), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), Record (Record), SourceInfo (SourceInfo), SourcePos (SourcePos), Sum (Sum), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, ProductBody, RecordBody, SumBody), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName), kwTyDefOpaque, tyBodyToTyDefKw)
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, encloseSep, equals, group, hsep, lbrace, line, lparen, pipe, rbrace, rparen, space, (<+>))
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
        <> case mayAl of
          Nothing -> ""
          Just al -> space <> "as" <+> pretty al
        <> case maySyms of
          Nothing -> ""
          Just syms -> space <> encloseSep lparen rparen comma (pretty <$> syms)

instance Pretty info => Pretty (TyDef info) where
  pretty (TyDef tn args Opaque _info) = pretty kwTyDefOpaque <+> pretty tn <> if null args then "" else space <> hsep (pretty <$> args)
  pretty (TyDef tn args body _info) = group $ pretty (tyBodyToTyDefKw body) <+> pretty tn <+> prettyTyAbs args body

prettyTyAbs :: Pretty info => [TyArg info] -> TyBody info -> Doc ann
prettyTyAbs [] body = equals <+> pretty body
prettyTyAbs args body = hsep (pretty <$> args) <+> equals <+> pretty body

instance Pretty info => Pretty (TyBody info) where
  pretty (SumBody s) = pretty s
  pretty (ProductBody p) = pretty p
  pretty (RecordBody r) = pretty r
  pretty Opaque = mempty

instance Pretty info => Pretty (Record info) where
  pretty (Record fields _info) =
    align $
      recordIntro
        <> encloseSep "" "" (comma <> space) (pretty <$> fields)
        <> recordOutro
    where
      recordIntro :: Doc ann
      recordIntro = lbrace <> space
      recordOutro :: Doc ann
      recordOutro = space <> rbrace

instance Pretty info => Pretty (Field info) where
  pretty (Field fn ty _info) = group $ pretty fn <+> colon <+> prettyTyTopLevel ty

instance Pretty info => Pretty (Product info) where
  pretty (Product fields _info) = align $ hsep (prettyTyInner <$> fields)

instance Pretty info => Pretty (Sum info) where
  pretty (Sum cs _info) = align $ encloseSep "" "" (space <> pipe <> space) (pretty <$> cs)

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

prettyTyInner :: Pretty info => Ty info -> Doc ann
prettyTyInner (TyVar vn) = pretty vn
prettyTyInner (TyRef' tr _info) = pretty tr
prettyTyInner (TyApp tyF tyAs _info) = group $ encloseSep lparen rparen space (prettyTyInner <$> tyF : tyAs)

prettyTyTopLevel :: Pretty info => Ty info -> Doc ann
prettyTyTopLevel (TyVar vn) = pretty vn
prettyTyTopLevel (TyRef' tr _info) = pretty tr
prettyTyTopLevel (TyApp tyF tyAs _info) = group $ hsep (prettyTyInner <$> tyF : tyAs)

instance Pretty info => Pretty (Constructor info) where
  pretty (Constructor cn (Product [] _) _info) = pretty cn
  pretty (Constructor cn p _info) = group $ hsep [pretty cn, pretty p]

instance Pretty SourceInfo where
  pretty (SourceInfo fn pos pos') = pretty fn <> ":" <> "(" <> pretty pos <> ")-(" <> pretty pos' <> ")"

instance Pretty SourcePos where
  pretty (SourcePos r c) = pretty r <> ":" <> pretty c

instance Pretty Parsec.ParseError where
  pretty pe = pretty (Parsec.errorPos pe) <> ":" <> pretty (Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ Parsec.errorMessages pe)

instance Pretty Parsec.SourcePos where
  pretty sp = pretty (Parsec.sourceName sp) <> ":" <> "(" <> pretty (Parsec.sourceLine sp) <> ":" <> pretty (Parsec.sourceColumn sp) <> ")"
