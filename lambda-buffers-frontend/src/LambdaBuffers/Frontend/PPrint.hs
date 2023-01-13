{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Frontend.PPrint () where

import Data.Text qualified as List
import LambdaBuffers.Frontend.Syntax (ClassName (ClassName), ConstrName (ConstrName), FieldName (FieldName), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), SourceInfo (SourceInfo), SourcePos (SourcePos), Ty (TyApp, TyRef', TyVar), TyName (TyName), TyRef (TyRef), VarName (VarName))
import Prettyprinter (Pretty (pretty), encloseSep, lbracket, rbracket, space)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec

instance Pretty info => Pretty (ModuleName info) where
  pretty (ModuleName ps _info) = pretty $ List.intercalate "." [p | ModuleNamePart p _ <- ps]

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
  pretty (TyApp tyF tyAs _info) = encloseSep lbracket rbracket space (pretty <$> tyF : tyAs)

instance Pretty SourceInfo where
  pretty (SourceInfo fn pos pos') = pretty fn <> ":" <> "(" <> pretty pos <> ")-(" <> pretty pos' <> ")"

instance Pretty SourcePos where
  pretty (SourcePos r c) = pretty r <> ":" <> pretty c

instance Pretty Parsec.ParseError where
  pretty pe = pretty (Parsec.errorPos pe) <> ":" <> pretty (Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ Parsec.errorMessages pe)

instance Pretty Parsec.SourcePos where
  pretty sp = pretty (Parsec.sourceName sp) <> ":" <> "(" <> pretty (Parsec.sourceLine sp) <> ":" <> pretty (Parsec.sourceColumn sp) <> ")"

-- data Constructor info = Constructor (ConstrName info) (Product info) info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- data Product info = Product [Ty info] info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- data TyArg info = TyArg Text info deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- data ModuleAlias info = ModuleAlias (ModuleName info) info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- data TyRef info = TyRef (Maybe (ModuleAlias info)) (TyName info) info deriving stock (Eq, Ord, Functor, Foldable, Traversable)
