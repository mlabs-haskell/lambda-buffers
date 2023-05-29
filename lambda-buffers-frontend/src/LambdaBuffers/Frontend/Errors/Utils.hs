module LambdaBuffers.Frontend.Errors.Utils (prettyClassRef, prettyTyRef, prettyKind, toSyntaxSourceInfo, prettyConstraint, prettyModuleName', prettyName', prettySourceInfo) where

import Control.Lens ((^.))
import Data.ProtoLens.Field (HasField)
import Data.Text (Text)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Syntax
import Prettyprinter (Doc, Pretty (pretty), parens, squotes, (<+>))
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

toSyntaxTy :: Lang.Ty -> Either Text (Syntax.Ty ())
toSyntaxTy langTy = case langTy ^. Lang.maybe'ty of
  Nothing -> Left "TODO(bladyjoker): Something went wrong when parsing a Lang Ty message"
  Just langTy' -> case langTy' of
    Lang.Ty'TyVar tv -> return $ Syntax.TyVar (Syntax.VarName (tv ^. (Lang.varName . Lang.name)) ())
    Lang.Ty'TyRef tr -> Syntax.TyRef' <$> toSyntaxTyRef tr <*> pure ()
    Lang.Ty'TyApp ta -> Syntax.TyApp <$> toSyntaxTy (ta ^. Lang.tyFunc) <*> (toSyntaxTy `traverse` (ta ^. Lang.tyArgs)) <*> pure ()

toSyntaxTyRef :: Lang.TyRef -> Either Text (Syntax.TyRef ())
toSyntaxTyRef tr = case tr ^. Lang.maybe'tyRef of
  Nothing -> Left "TODO(bladyjoker): Something went wrong when parsing a Lang TyRef message"
  Just tr' -> case tr' of
    Lang.TyRef'ForeignTyRef ftr ->
      return $
        Syntax.TyRef
          (toSyntaxModuleAlias (ftr ^. Lang.moduleName))
          (Syntax.TyName (ftr ^. (Lang.tyName . Lang.name)) ())
          ()
    Lang.TyRef'LocalTyRef ltr ->
      return $
        Syntax.TyRef
          Nothing
          (Syntax.TyName (ltr ^. (Lang.tyName . Lang.name)) ())
          ()

toSyntaxClassRef :: Lang.TyClassRef -> Either Text (Syntax.ClassRef ())
toSyntaxClassRef cr = case cr ^. Lang.maybe'classRef of
  Nothing -> Left "TODO(bladyjoker): Something went wrong when parsing a Lang TyClassRef message"
  Just cr' -> case cr' of
    Lang.TyClassRef'ForeignClassRef fcr ->
      return $
        Syntax.ClassRef
          (toSyntaxModuleAlias (fcr ^. Lang.moduleName))
          (Syntax.ClassName (fcr ^. (Lang.className . Lang.name)) ())
          ()
    Lang.TyClassRef'LocalClassRef lcr ->
      return $
        Syntax.ClassRef
          Nothing
          (Syntax.ClassName (lcr ^. (Lang.className . Lang.name)) ())
          ()

toSyntaxModuleAlias :: Lang.ModuleName -> Maybe (Syntax.ModuleAlias ())
toSyntaxModuleAlias mn = Just $ Syntax.ModuleAlias (toSyntaxModuleName mn) ()

toSyntaxModuleName :: Lang.ModuleName -> Syntax.ModuleName ()
toSyntaxModuleName mn = Syntax.ModuleName [Syntax.ModuleNamePart (p ^. Lang.name) () | p <- mn ^. Lang.parts] ()

toSyntaxSourceInfo :: Lang.SourceInfo -> Syntax.SourceInfo
toSyntaxSourceInfo si =
  Syntax.SourceInfo
    (si ^. Lang.file)
    ( Syntax.SourcePos
        (fromIntegral $ si ^. (Lang.posFrom . Lang.row))
        (fromIntegral $ si ^. (Lang.posFrom . Lang.column))
    )
    ( Syntax.SourcePos
        (fromIntegral $ si ^. (Lang.posTo . Lang.row))
        (fromIntegral $ si ^. (Lang.posTo . Lang.column))
    )

toSyntaxConstraint :: Lang.Constraint -> Either Text (Syntax.Constraint ())
toSyntaxConstraint langCstr = Syntax.Constraint <$> toSyntaxClassRef (langCstr ^. Lang.classRef) <*> toSyntaxTy `traverse` (langCstr ^. Lang.args) <*> pure ()

prettyConstraint :: Lang.Constraint -> Doc ann
prettyConstraint cstr = either pretty pretty (toSyntaxConstraint cstr)

prettyModuleName :: Lang.ModuleName -> Doc ann
prettyModuleName = pretty . toSyntaxModuleName

prettyModuleName' :: Lang.ModuleName -> Doc ann
prettyModuleName' = squotes . prettyModuleName

prettyClassRef :: Lang.TyClassRef -> Doc ann
prettyClassRef cr = either pretty pretty (toSyntaxClassRef cr)

prettyTyRef :: Lang.TyRef -> Doc ann
prettyTyRef cr = either pretty pretty (toSyntaxTyRef cr)

prettyKind :: Lang.Kind -> Doc ann
prettyKind k = case k ^. Lang.maybe'kind of
  Nothing -> "TODO(bladyjoker): Error while printing Kind"
  Just k' -> case k' of
    Lang.Kind'KindRef kr -> case kr of
      Lang.Kind'KIND_REF_TYPE -> "*"
      _ -> "TODO(bladyjoker): Something went wrong when printing a KindRef"
    Lang.Kind'KindArrow' ka -> parens (prettyKind (ka ^. Lang.left) <+> "->" <+> prettyKind (ka ^. Lang.right))

prettyName :: forall {a} {s} {ann}. (Pretty a, HasField s "name" a) => s -> Doc ann
prettyName x = pretty (x ^. Lang.name)

prettyName' :: forall {a} {s} {ann}. (Pretty a, HasField s "name" a) => s -> Doc ann
prettyName' = squotes . prettyName

prettySourceInfo :: Lang.SourceInfo -> Doc ann
prettySourceInfo = pretty . toSyntaxSourceInfo
