module LambdaBuffers.Compiler.MiniLog.Pretty (toPrologModule) where

import Data.Char (toLower)
import Data.List (sortBy)
import Data.List qualified as List
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.MiniLog (Clause (MkClause, clauseHead), Term (Atom, Struct, Var))
import Prettyprinter (Doc, Pretty (pretty), align, comma, dot, encloseSep, hardline, lbracket, line, lparen, rbracket, rparen, slash, space, squote, (<+>))

toPrologModule :: (Show f, Show a, Ord f, Ord a) => Text -> [Clause f a] -> (FilePath, String)
toPrologModule moduleName clauses = show <$> prettyModule moduleName clauses

prettyModule :: (Show f, Show a, Ord f, Ord a) => Text -> [Clause f a] -> (FilePath, Doc ann)
prettyModule moduleName clauses =
  ( toFilepath moduleName
  , ":- module"
      <> lparen
      <> prettyModuleName moduleName
      <> comma
      <> prettyExports clauses
      <> rparen
      <> dot
      <> line
      <> line
      <> prettyClauses clauses
      <> line
  )

prettyClauses :: (Show f, Show a, Ord f, Ord a) => [Clause f a] -> Doc ann
prettyClauses = encloseSep mempty mempty (hardline <> hardline) . fmap prettyClause . sortBy (comparing Data.Ord.Down)

prettyModuleName :: Text -> Doc ann
prettyModuleName = prettyAtom

prettyExports :: (Show f, Ord f) => [Clause f a] -> Doc ann
prettyExports clauses =
  let sigs =
        List.nub
          [ (f, length args)
          | Struct f args <- clauseHead <$> clauses
          ]
      exportDocs = [prettyAtom f <> slash <> pretty arity | (f, arity) <- sigs]
   in lbracket <> align (encloseSep mempty rbracket comma exportDocs)

toFilepath :: Text -> FilePath
toFilepath = fmap ((\c -> if c == '.' then '_' else c) . toLower) . Text.unpack

-- | Printing to Prolog terms.
prettyTerm :: (Show f, Show a) => Term f a -> Doc ann
prettyTerm (Atom at) = prettyAtom at
prettyTerm (Struct f []) = prettyAtom f
prettyTerm (Struct f args) = prettyAtom f <> align (lparen <> encloseSep mempty rparen comma (prettyTerm <$> args))
prettyTerm (Var vn) = "V" <> pretty vn

prettyAtom :: Show a => a -> Doc annnn
prettyAtom at = squote <> (pretty . filter (/= '"') . show $ at) <> squote

prettyClause :: (Show f, Show a) => Clause f a -> Doc ann
prettyClause (MkClause headt []) = prettyTerm headt <> dot
prettyClause (MkClause headt bodyts) = prettyTerm headt <+> ":-" <> line <> space <> space <> align (encloseSep mempty mempty comma (prettyTerm <$> bodyts) <> dot)
