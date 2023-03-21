module LambdaBuffers.Frontend.Errors (
  FrontendError (..),
  importedNotFoundErr,
  tyDefNameConflictErr,
  classDefNameConflictErr,
  duplicateClassDefErr,
  duplicateTyDefErr,
  tyNameAlreadyImportedErr,
  classNameAlreadyImportedErr,
  tyRefNotFoundErr,
  classRefNotFoundErr,
) where

import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (ClassDef (ClassDef, classInfo, className), ClassName (ClassName), ClassRef (ClassRef), Import (importInfo, importModuleName), ModuleName (ModuleName), Name (Name), SourceInfo, TyDef (TyDef, tyDefInfo, tyName), TyName (TyName), TyRef (TyRef))
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, prettyClassSym, prettyTySym)
import Prettyprinter (Doc, LayoutOptions (layoutPageWidth), PageWidth (Unbounded), Pretty (pretty), align, brackets, defaultLayoutOptions, group, layoutPretty, sep, (<+>))
import Prettyprinter.Render.String (renderShowS)
import Text.Parsec (ParseError)

data FrontendError
  = ModuleNotFound (ModuleName SourceInfo) (Import SourceInfo) [FilePath]
  | MultipleModulesFound (ModuleName SourceInfo) (Import SourceInfo) [FilePath]
  | ImportCycleFound (ModuleName SourceInfo) (Import SourceInfo) [ModuleName ()]
  | ModuleParseError FilePath ParseError
  | ImportedNotFound ImportedNotFound
  | InvalidModuleFilepath (ModuleName SourceInfo) FilePath FilePath
  | TyRefNotFound TyRefNotFound
  | ClassRefNotFound ClassRefNotFound
  | TyNameAlreadyImported TyNameAlreadyImported
  | ClassNameAlreadyImported ClassNameAlreadyImported
  | DuplicateTyDef DuplicateTyDef
  | DuplicateClassDef DuplicateClassDef
  | TyDefNameConflict TyDefNameConflict
  | ClassDefNameConflict ClassDefNameConflict
  deriving stock (Eq)

instance Show FrontendError where
  show (ModuleNotFound _cm imp impPaths) = showOneLine $ pretty (importInfo imp) <+> "Module" <+> pretty (importModuleName imp) <+> "not found in available import paths" <+> pretty impPaths
  show (MultipleModulesFound _cm imp conflictingPaths) = showOneLine $ pretty (importInfo imp) <+> "Module" <+> pretty (importModuleName imp) <+> "found in multiple files" <+> pretty conflictingPaths
  show (ImportCycleFound _cm imp visited) = showOneLine $ pretty (importInfo imp) <+> "Tried to load module" <+> pretty (importModuleName imp) <+> "which constitutes a cycle" <+> pretty visited
  show (ModuleParseError _fp err) = showOneLine $ pretty err
  show (InvalidModuleFilepath mn@(ModuleName _ info) gotModFp wantedFpSuffix) = showOneLine $ pretty info <+> "File name" <+> pretty gotModFp <+> "doesn't match module name" <+> pretty mn <+> "expected" <+> pretty wantedFpSuffix
  show (TyRefNotFound err) = showOneLine $ prettyTyRefNotFound err
  show (ClassRefNotFound err) = showOneLine $ prettyClassRefNotFound err
  show (ImportedNotFound err) = showOneLine $ prettyImportedNotFound err
  show (TyNameAlreadyImported err) = showOneLine $ prettyTyNameAlreadyImported err
  show (ClassNameAlreadyImported err) = showOneLine $ prettyClassNameAlreadyImported err
  show (DuplicateTyDef err) = showOneLine $ prettyDuplicateTyDef err
  show (DuplicateClassDef err) = showOneLine $ prettyDuplicateClassDef err
  show (TyDefNameConflict err) = showOneLine $ prettyTyDefNameConflict err
  show (ClassDefNameConflict err) = showOneLine $ prettyClassDefNameConflict err

showOneLine :: Doc a -> String
showOneLine d = (renderShowS . layoutPretty (defaultLayoutOptions {layoutPageWidth = Unbounded}) $ d) ""

prettyContext :: ModuleName SourceInfo -> SourceInfo -> Doc ann
prettyContext currentModuleName sourceInfo = pretty sourceInfo <+> brackets (pretty currentModuleName)

data ImportedNotFound
  = MkImportedNotFound
      (ModuleName SourceInfo)
      (ModuleName SourceInfo)
      (Name SourceInfo)
      (Set (TyName ()), Set (ClassName ()))
  deriving stock (Eq)

importedNotFoundErr ::
  ModuleName SourceInfo ->
  ModuleName SourceInfo ->
  Name SourceInfo ->
  (Set (TyName ()), Set (ClassName ())) ->
  FrontendError
importedNotFoundErr cm im n is = ImportedNotFound $ MkImportedNotFound cm im n is

prettyImportedNotFound :: forall {ann}. ImportedNotFound -> Doc ann
prettyImportedNotFound (MkImportedNotFound cMn impMn n@(Name _ info) (tyNs, clNs)) =
  prettyContext cMn info
    <+> "Name"
    <+> pretty n
    <+> "not found in module"
    <+> pretty impMn
      <> ( ", did you mean one of the types:"
            <+> (align . group . sep $ (pretty <$> toList tyNs))
              <> ". Or did you mean one of the classes:"
            <+> (align . group . sep $ (pretty <$> toList clNs))
         )

data TyDefNameConflict
  = MkTyDefNameConflict
      (ModuleName SourceInfo)
      (TyDef SourceInfo)
      (ModuleName ())
  deriving stock (Eq)

tyDefNameConflictErr ::
  ModuleName SourceInfo ->
  TyDef SourceInfo ->
  ModuleName () ->
  FrontendError
tyDefNameConflictErr cm td impMn = TyDefNameConflict $ MkTyDefNameConflict cm td impMn

prettyTyDefNameConflict :: TyDefNameConflict -> Doc ann
prettyTyDefNameConflict (MkTyDefNameConflict cMn (TyDef tyn@(TyName _ info) _ _ _) imn) =
  prettyContext cMn info
    <+> "Type name"
    <+> pretty tyn
    <+> "conflicts with an imported type name from module"
    <+> pretty imn

data ClassDefNameConflict
  = MkClassDefNameConflict
      (ModuleName SourceInfo)
      (ClassDef SourceInfo)
      (ModuleName ())
  deriving stock (Eq)

classDefNameConflictErr ::
  ModuleName SourceInfo ->
  ClassDef SourceInfo ->
  ModuleName () ->
  FrontendError
classDefNameConflictErr cm cd impMn = ClassDefNameConflict $ MkClassDefNameConflict cm cd impMn

prettyClassDefNameConflict :: ClassDefNameConflict -> Doc ann
prettyClassDefNameConflict (MkClassDefNameConflict cMn (ClassDef tyn@(ClassName _ info) _ _ _) imn) =
  prettyContext cMn info
    <+> "Class name"
    <+> pretty tyn
    <+> "conflicts with an imported class name from module"
    <+> pretty imn

data DuplicateTyDef
  = MkDuplicateTyDef (ModuleName SourceInfo) (TyDef SourceInfo)
  deriving stock (Eq)

duplicateTyDefErr :: ModuleName SourceInfo -> TyDef SourceInfo -> FrontendError
duplicateTyDefErr cMn td = DuplicateTyDef $ MkDuplicateTyDef cMn td

prettyDuplicateTyDef :: DuplicateTyDef -> Doc ann
prettyDuplicateTyDef (MkDuplicateTyDef cMn tyDef) =
  prettyContext cMn (tyDefInfo tyDef)
    <+> "Duplicate type definition with the name"
    <+> pretty (tyName tyDef)

data DuplicateClassDef
  = MkDuplicateClassDef (ModuleName SourceInfo) (ClassDef SourceInfo)
  deriving stock (Eq)

duplicateClassDefErr :: ModuleName SourceInfo -> ClassDef SourceInfo -> FrontendError
duplicateClassDefErr cMn td = DuplicateClassDef $ MkDuplicateClassDef cMn td

prettyDuplicateClassDef :: DuplicateClassDef -> Doc ann
prettyDuplicateClassDef (MkDuplicateClassDef cMn classDef) =
  prettyContext cMn (classInfo classDef)
    <+> "Duplicate class definition with the name"
    <+> pretty (className classDef)

data TyNameAlreadyImported
  = MkTyNameAlreadyImported
      (ModuleName SourceInfo)
      (Import SourceInfo)
      (TyName ())
      (ModuleName ())
  deriving stock (Eq)

tyNameAlreadyImportedErr ::
  ModuleName SourceInfo ->
  Import SourceInfo ->
  TyName () ->
  ModuleName () ->
  FrontendError
tyNameAlreadyImportedErr cm imp tn amn = TyNameAlreadyImported $ MkTyNameAlreadyImported cm imp tn amn

prettyTyNameAlreadyImported :: TyNameAlreadyImported -> Doc ann
prettyTyNameAlreadyImported (MkTyNameAlreadyImported cMn impStmt tyN alreadyInMn) =
  prettyContext cMn (importInfo impStmt)
    <+> "Type name"
    <+> pretty tyN
    <+> "already imported from module"
    <+> pretty alreadyInMn

data ClassNameAlreadyImported
  = MkClassNameAlreadyImported
      (ModuleName SourceInfo)
      (Import SourceInfo)
      (ClassName ())
      (ModuleName ())
  deriving stock (Eq)

classNameAlreadyImportedErr ::
  ModuleName SourceInfo ->
  Import SourceInfo ->
  ClassName () ->
  ModuleName () ->
  FrontendError
classNameAlreadyImportedErr cm imp tn amn = ClassNameAlreadyImported $ MkClassNameAlreadyImported cm imp tn amn

prettyClassNameAlreadyImported :: ClassNameAlreadyImported -> Doc ann
prettyClassNameAlreadyImported (MkClassNameAlreadyImported cMn impStmt clN alreadyInMn) =
  prettyContext cMn (importInfo impStmt)
    <> "Class name"
    <+> pretty clN
    <+> "already imported from module"
    <+> pretty alreadyInMn

data TyRefNotFound = MkTyRefNotFound (ModuleName SourceInfo) (TyRef SourceInfo) TyScope deriving stock (Eq)

tyRefNotFoundErr ::
  ModuleName SourceInfo ->
  TyRef SourceInfo ->
  TyScope ->
  FrontendError
tyRefNotFoundErr mn tr s = TyRefNotFound $ MkTyRefNotFound mn tr s

prettyTyRefNotFound :: forall {ann}. TyRefNotFound -> Doc ann
prettyTyRefNotFound (MkTyRefNotFound cMn tyR@(TyRef _ _ info) scope) =
  prettyContext cMn info
    <+> "Type " <> pretty tyR
    <+> "not found in the module's scope"
    <+> (align . group . sep . fmap prettyTySym . Map.keys $ scope)

data ClassRefNotFound = MkClassRefNotFound (ModuleName SourceInfo) (ClassRef SourceInfo) ClassScope deriving stock (Eq)

classRefNotFoundErr ::
  ModuleName SourceInfo ->
  ClassRef SourceInfo ->
  ClassScope ->
  FrontendError
classRefNotFoundErr mn cr s = ClassRefNotFound $ MkClassRefNotFound mn cr s

prettyClassRefNotFound :: forall {ann}. ClassRefNotFound -> Doc ann
prettyClassRefNotFound (MkClassRefNotFound cMn clR@(ClassRef _ _ info) scope) =
  prettyContext cMn info
    <+> "Class " <> pretty clR
    <+> "not found in the module's scope"
    <+> (align . group . sep . fmap prettyClassSym . Map.keys $ scope)
