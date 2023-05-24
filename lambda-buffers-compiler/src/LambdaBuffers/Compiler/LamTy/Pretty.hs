{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Compiler.LamTy.Pretty (prettyTy) where

import Control.Lens (view, (^.))
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Compiler.LamTy.Types (Ty (TyAbs, TyApp, TyOpaque, TyProduct, TyRecord, TyRef, TySum, TyVar))
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Utils (prettyModuleName)
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), Pretty (pretty), colon, comma, concatWith, dot, enclose, encloseSep, hsep, layoutPretty, lbrace, lparen, pipe, rbrace, rparen, space, (<+>))
import Prettyprinter.Render.String (renderShowS)

prettyTy :: Ty -> Doc a
prettyTy (TyVar tv) = pretty $ tv ^. #varName . #name
prettyTy (TyAbs args body _) =
  if null args
    then enclose lparen rparen $ "\\" <> prettyTy body
    else enclose lparen rparen $ printArgs args <+> "->" <+> prettyTy body
  where
    printArgs :: OMap (PC.InfoLess PC.VarName) PC.TyArg -> Doc a
    printArgs args' = hsep (pretty . view (#argName . #name) <$> toList args')
prettyTy (TyApp f args _t) = if null args then prettyTy f else encloseSep lparen rparen space (prettyTy <$> f : args)
prettyTy (TySum ctors _s) = enclose lparen rparen $ sepWith (space <> pipe <> space) (fmap prettyCtor . OMap.assocs $ ctors)
  where
    prettyCtor :: (PC.InfoLess PC.ConstrName, Ty) -> Doc a
    prettyCtor (cn, p) = pretty (PC.withInfoLess cn (view #name)) <+> prettyTy p
prettyTy (TyProduct fields _t) = hsep $ prettyTy <$> fields
prettyTy (TyRecord fields _r) = encloseSep lbrace rbrace comma $ prettyField <$> OMap.assocs fields
prettyTy (TyOpaque _) = "opq"
prettyTy (TyRef (PC.LocalI lr)) = pretty $ lr ^. #tyName . #name
prettyTy (TyRef (PC.ForeignI fr)) = prettyModuleName (fr ^. #moduleName) <> dot <> pretty (fr ^. #tyName . #name)

prettyField :: (PC.InfoLess PC.FieldName, Ty) -> Doc ann
prettyField (fn, ty) = PC.withInfoLess fn (pretty . view #name) <+> colon <+> prettyTy ty

sepWith :: Foldable t => Doc ann -> t (Doc ann) -> Doc ann
sepWith d = concatWith (\l r -> l <> d <> r)

instance Pretty Ty where
  pretty = prettyTy

instance Show Ty where
  showsPrec _ = renderShowS . layoutPretty (LayoutOptions Unbounded) . pretty
