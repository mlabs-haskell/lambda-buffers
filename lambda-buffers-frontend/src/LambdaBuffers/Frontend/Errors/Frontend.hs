module LambdaBuffers.Frontend.Errors.Frontend (
  -- * Error types and smart constructors
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

  -- * Pretty printers  / utilities
  prettyFrontendErrorMessage,
  prettyModuleAndSourceInfoContext,
  getFrontendModuleAndSourceInfoContext,
) where

import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text qualified as Text
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (ClassDef (ClassDef, classInfo, className), ClassName (ClassName), ClassRef (ClassRef), Import (importInfo, importModuleName), ModuleName (ModuleName), Name (Name), SourceInfo (SourceInfo), SourcePos (SourcePos), TyDef (TyDef, tyDefInfo, tyName), TyName (TyName), TyRef (TyRef))
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, prettyClassSym, prettyTySym)
import Prettyprinter (Doc, LayoutOptions (layoutPageWidth), PageWidth (Unbounded), Pretty (pretty), align, brackets, defaultLayoutOptions, group, layoutPretty, sep, (<+>))
import Prettyprinter.Render.String (renderShowS)
import Text.Parsec (ParseError)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec.Error

{- | A pretty printer for a 'FrontendError' error message which doesn't include
a location information prefix e.g. this function would output @super bad
error@  instead of something like @myFile:1.1: super bad error@
-}
prettyFrontendErrorMessage :: FrontendError -> Doc a
prettyFrontendErrorMessage = \case
  ModuleNotFound _cm imp impPaths ->
    "module" <+> pretty (importModuleName imp) <+> "not found in available import paths" <+> pretty impPaths
  MultipleModulesFound _cm imp conflictingPaths -> "module" <+> pretty (importModuleName imp) <+> "found in multiple files" <+> pretty conflictingPaths
  ImportCycleFound _cm imp visited -> "tried to load module" <+> pretty (importModuleName imp) <+> "which constitutes a cycle" <+> pretty visited
  ModuleParseError _fp err -> pretty (Parsec.Error.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ Parsec.Error.errorMessages err)
  InvalidModuleFilepath mn gotModFp wantedFpSuffix ->
    "file name" <+> pretty gotModFp <+> "doesn't match module name" <+> pretty mn <+> "expected" <+> pretty wantedFpSuffix
  TyRefNotFound (MkTyRefNotFound _cMn tyR scope) ->
    "type"
      <+> pretty tyR
      <+> "not found in the module's scope"
      <+> (align . group . sep . fmap prettyTySym . Map.keys $ scope)
  ClassRefNotFound (MkClassRefNotFound _cMn clR scope) ->
    "class" <+> pretty clR <+> "not found in the module's scope" <+> (align . group . sep . fmap prettyClassSym . Map.keys $ scope)
  ImportedNotFound (MkImportedNotFound _cMn impMn n (tyNs, clNs)) ->
    "name"
      <+> pretty n
      <+> "not found in module"
      <+> pretty impMn
      <> ( ", did you mean one of the types:"
            <+> (align . group . sep $ (pretty <$> toList tyNs))
            <> ". Or did you mean one of the classes:"
            <+> (align . group . sep $ (pretty <$> toList clNs))
         )
  TyNameAlreadyImported (MkTyNameAlreadyImported _cMn _impStmt tyN alreadyInMn) ->
    "type name" <+> pretty tyN <+> "already imported from module" <+> pretty alreadyInMn
  ClassNameAlreadyImported (MkClassNameAlreadyImported _cMn _impStmt clN alreadyInMn) ->
    "class name" <+> pretty clN <+> "already imported from module" <+> pretty alreadyInMn
  DuplicateTyDef (MkDuplicateTyDef _cMn tyDef) -> "duplicate type definition with the name" <+> pretty (tyName tyDef)
  DuplicateClassDef (MkDuplicateClassDef _cMn classDef) ->
    "duplicate class definition with the name" <+> pretty (className classDef)
  TyDefNameConflict (MkTyDefNameConflict _cMn (TyDef tyn _ _ _) imn) ->
    "type name"
      <+> pretty tyn
      <+> "conflicts with an imported type name from module"
      <+> pretty imn
  ClassDefNameConflict (MkClassDefNameConflict _cMn (ClassDef tyn _ _ _) imn) ->
    "class name"
      <+> pretty tyn
      <+> "conflicts with an imported class name from module"
      <+> pretty imn

{- | 'getFrontendModuleAndSourceInfoContext' grabs the 'ModuleName' / 'SourceInfo' context of the
'FrontendError'. This is useful for error messages.
Note: in the case of a parser error, the module name may not be known, hence
why that is a 'Maybe'
-}
getFrontendModuleAndSourceInfoContext :: FrontendError -> (Maybe (ModuleName SourceInfo), SourceInfo)
getFrontendModuleAndSourceInfoContext = \case
  ModuleNotFound cm imp _impPaths -> (Just cm, importInfo imp)
  MultipleModulesFound cm imp _conflictingPaths -> (Just cm, importInfo imp)
  ImportCycleFound cm imp _visited -> (Just cm, importInfo imp)
  ModuleParseError _fp err ->
    ( Nothing
    , let parsecErrorPos = Parsec.errorPos err
       in SourceInfo
            (Text.pack $ Parsec.sourceName parsecErrorPos)
            (SourcePos (Parsec.sourceLine parsecErrorPos) (Parsec.sourceColumn parsecErrorPos))
            -- NOTE(jaredponn): the parser doesn't know the "span" of the
            -- error, so we copy pasta the same error position twice
            (SourcePos (Parsec.sourceLine parsecErrorPos) (Parsec.sourceColumn parsecErrorPos))
    )
  InvalidModuleFilepath mn@(ModuleName _ info) _gotModFp _wantedFpSuffix -> (Just mn, info)
  TyRefNotFound (MkTyRefNotFound cMn (TyRef _ _ info) _scope) -> (Just cMn, info)
  ClassRefNotFound (MkClassRefNotFound cMn (ClassRef _ _ info) _scope) -> (Just cMn, info)
  ImportedNotFound (MkImportedNotFound cMn _ (Name _ info) _) -> (Just cMn, info)
  TyNameAlreadyImported (MkTyNameAlreadyImported cMn impStmt _tyN _alreadyInMn) -> (Just cMn, importInfo impStmt)
  ClassNameAlreadyImported (MkClassNameAlreadyImported cMn impStmt _clN _alreadyInMn) -> (Just cMn, importInfo impStmt)
  DuplicateTyDef (MkDuplicateTyDef cMn tyDef) -> (Just cMn, tyDefInfo tyDef)
  DuplicateClassDef (MkDuplicateClassDef cMn classDef) -> (Just cMn, classInfo classDef)
  TyDefNameConflict (MkTyDefNameConflict cMn (TyDef (TyName _ info) _ _ _) _imn) ->
    (Just cMn, info)
  ClassDefNameConflict (MkClassDefNameConflict cMn (ClassDef (ClassName _ info) _ _ _) _imn) ->
    (Just cMn, info)

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
  show frontendError =
    showOneLine $
      uncurry prettyModuleAndSourceInfoContext (getFrontendModuleAndSourceInfoContext frontendError)
        <+> prettyFrontendErrorMessage frontendError

showOneLine :: Doc a -> String
showOneLine d = (renderShowS . layoutPretty (defaultLayoutOptions {layoutPageWidth = Unbounded}) $ d) ""

{- | Creates a doc of either of the forms:

@filename: 2.3: (module Some.Module.Name)@
or
@filename: 2.3:@
-}
prettyModuleAndSourceInfoContext :: Maybe (ModuleName SourceInfo) -> SourceInfo -> Doc ann
prettyModuleAndSourceInfoContext (Just currentModuleName) sourceInfo = pretty sourceInfo <> ":" <+> brackets ("module" <+> pretty currentModuleName)
prettyModuleAndSourceInfoContext Nothing sourceInfo = pretty sourceInfo <> ":"

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

data DuplicateTyDef
  = MkDuplicateTyDef (ModuleName SourceInfo) (TyDef SourceInfo)
  deriving stock (Eq)

duplicateTyDefErr :: ModuleName SourceInfo -> TyDef SourceInfo -> FrontendError
duplicateTyDefErr cMn td = DuplicateTyDef $ MkDuplicateTyDef cMn td

data DuplicateClassDef
  = MkDuplicateClassDef (ModuleName SourceInfo) (ClassDef SourceInfo)
  deriving stock (Eq)

duplicateClassDefErr :: ModuleName SourceInfo -> ClassDef SourceInfo -> FrontendError
duplicateClassDefErr cMn td = DuplicateClassDef $ MkDuplicateClassDef cMn td

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

data TyRefNotFound = MkTyRefNotFound (ModuleName SourceInfo) (TyRef SourceInfo) TyScope deriving stock (Eq)

tyRefNotFoundErr ::
  ModuleName SourceInfo ->
  TyRef SourceInfo ->
  TyScope ->
  FrontendError
tyRefNotFoundErr mn tr s = TyRefNotFound $ MkTyRefNotFound mn tr s

data ClassRefNotFound = MkClassRefNotFound (ModuleName SourceInfo) (ClassRef SourceInfo) ClassScope deriving stock (Eq)

classRefNotFoundErr ::
  ModuleName SourceInfo ->
  ClassRef SourceInfo ->
  ClassScope ->
  FrontendError
classRefNotFoundErr mn cr s = ClassRefNotFound $ MkClassRefNotFound mn cr s
