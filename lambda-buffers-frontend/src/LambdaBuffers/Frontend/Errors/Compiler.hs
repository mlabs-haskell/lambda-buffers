{- | Compiler error printing

Why is this here and not in the Compiler? It's because errors must be presented
in terms of the Frontend constructs. For instance, when printing out a type
expressions or a constraint. We're reusing the PPrint module available in the
Frontend to print out these concepts when they are needed in the error.

Another example of where this could be even better is if the reporting also had
a Frontend module context such that we can restore original module aliases with
type references where they are indeed used.
-}
module LambdaBuffers.Frontend.Errors.Compiler (toErrors) where

import Control.Lens ((^.))
import Data.ProtoLens.Field (HasField)
import Data.Text (Text)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Syntax
import Prettyprinter (Doc, Pretty (pretty), dot, encloseSep, line, parens, squotes, vsep, (<+>))
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

toErrors :: Compiler.Error -> [(Maybe Lang.SourceInfo, Doc ann)]
toErrors err =
  protoParseErrors (err ^. Compiler.protoParseErrors)
    <> internalErrors (err ^. Compiler.internalErrors)
    <> kindCheckErrors (err ^. Compiler.kindCheckErrors)
    <> classCheckErrors (err ^. Compiler.tyClassCheckErrors)
    <> namingCheckErrors (err ^. Compiler.namingErrors)

namingCheckErrors :: [Compiler.NamingError] -> [(Maybe Lang.SourceInfo, Doc ann)]
namingCheckErrors errs = do
  err <- errs
  maybe [] namingCheckErrors' (err ^. Compiler.maybe'namingError)

internalErrors :: [Compiler.InternalError] -> [(Maybe Lang.SourceInfo, Doc ann)]
internalErrors errs = do
  err <- errs
  return (Just $ err ^. Compiler.sourceInfo, "Compiler reported an internal error:" <+> pretty (err ^. Compiler.msg))

protoParseErrors :: [Compiler.ProtoParseError] -> [(Maybe Lang.SourceInfo, Doc ann)]
protoParseErrors errs = do
  err <- errs
  maybe [] protoParseErrors' (err ^. Compiler.maybe'protoParseError)

kindCheckErrors :: [Compiler.KindCheckError] -> [(Maybe Lang.SourceInfo, Doc ann)]
kindCheckErrors errs = do
  err <- errs
  maybe [] kindCheckErrors' (err ^. Compiler.maybe'kindCheckError)

classCheckErrors :: [Compiler.TyClassCheckError] -> [(Maybe Lang.SourceInfo, Doc ann)]
classCheckErrors errs = do
  err <- errs
  maybe [] classCheckErrors' (err ^. Compiler.maybe'tyclassCheckError)

protoParseErrors' :: Compiler.ProtoParseError'ProtoParseError -> [(Maybe Lang.SourceInfo, Doc ann)]
protoParseErrors' (Compiler.ProtoParseError'MultipleClassdefError err) = do
  let mn = err ^. Compiler.moduleName
  cd <- err ^. Compiler.classDefs
  return
    ( Just $ cd ^. Lang.className . Lang.sourceInfo
    , "A class definition with a duplicate name" <+> prettyName' (cd ^. Lang.className) <+> "was defined in module" <+> prettyModuleName' mn
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleTydefError err) = do
  let mn = err ^. Compiler.moduleName
  td <- err ^. Compiler.tyDefs
  return
    ( Just $ td ^. Lang.tyName . Lang.sourceInfo
    , "A type definition with a duplicate name" <+> prettyName' (td ^. Lang.tyName) <+> "was defined in module" <+> prettyModuleName' mn
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleModuleError' err) = do
  m <- err ^. Compiler.modules
  return
    ( Just $ m ^. Lang.moduleName . Lang.sourceInfo
    , "A module with a duplicate name" <+> prettyModuleName' (m ^. Lang.moduleName) <+> "was defined"
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleTyargError err) = do
  let mn = err ^. Compiler.moduleName
      td = err ^. Compiler.tyDef
  arg <- err ^. Compiler.tyArgs
  return
    ( Just $ arg ^. Lang.argName . Lang.sourceInfo
    , "A type argument definition with a duplicate name"
        <+> prettyName' (arg ^. Lang.argName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName' (td ^. Lang.tyName)
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleConstructorError' err) = do
  let mn = err ^. Compiler.moduleName
      td = err ^. Compiler.tyDef
  ctor <- err ^. Compiler.constructors
  return
    ( Just $ ctor ^. Lang.constrName . Lang.sourceInfo
    , "A sum type constructor definitions with a duplicate name"
        <+> prettyName' (ctor ^. Lang.constrName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName' (td ^. Lang.tyName)
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleFieldError' err) = do
  let mn = err ^. Compiler.moduleName
      td = err ^. Compiler.tyDef
  field <- err ^. Compiler.fields
  return
    ( Just $ field ^. Lang.fieldName . Lang.sourceInfo
    , "A record type field definitions with a duplicate name"
        <+> prettyName' (field ^. Lang.fieldName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName' (td ^. Lang.tyName)
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleImportError' err) = do
  let mn = err ^. Compiler.moduleName
  importedMn <- err ^. Compiler.imports
  return
    ( Just $ importedMn ^. Lang.sourceInfo
    , "A duplicate import for"
        <+> prettyModuleName' importedMn
        <+> "was defined in module"
        <+> prettyModuleName' mn
          <> dot
        <+> "This generally means the Frontend sent a malformed request to the Compiler"
    )
-- TODO(bladyjoker): Add module name information at least.
protoParseErrors' (Compiler.ProtoParseError'OneOfNotSetError' err) =
  [
    ( Nothing
    , "While processing the Compiler Input request a `oneOf` ProtocolBuffers field type"
        <+> "in a message"
        <+> squotes (pretty (err ^. Compiler.messageName))
        <+> "and field "
        <+> squotes (pretty (err ^. Compiler.fieldName))
        <+> "was not set"
          <> dot
        <+> "This generally means the Frontend sent a malformed request to the Compiler"
    )
  ]
-- TODO(bladyjoker): Add module name information at least.
protoParseErrors' (Compiler.ProtoParseError'UnknownEnumError' err) =
  [
    ( Nothing
    , "While processing the Compiler Input request a `enum` ProtocolBuffers field type"
        <+> squotes (pretty (err ^. Compiler.enumName))
        <+> "was processed with an unknown `tag` number"
        <+> pretty (err ^. Compiler.gotTag)
          <> dot
        <+> "This generally means the Frontend sent a malformed request to the Compiler"
    )
  ]

kindCheckErrors' :: Compiler.KindCheckError'KindCheckError -> [(Maybe Lang.SourceInfo, Doc ann)]
kindCheckErrors' (Compiler.KindCheckError'UnboundTyRefError' err) = case err ^. (Compiler.tyRef . Lang.maybe'tyRef) of
  Nothing ->
    [
      ( Just $ err ^. (Compiler.moduleName . Lang.sourceInfo)
      , "Unexpected error occurred while trying to report a `KindCheckError.UnboundTyRefError` error. This likely means the Compiler responded with a malformed Compiler Output"
      )
    ]
  Just tr -> case tr of
    Lang.TyRef'ForeignTyRef ftr ->
      [
        ( Just $ ftr ^. (Lang.tyName . Lang.sourceInfo)
        , "An unbound foreign type reference"
            <+> prettyTyRef (err ^. Compiler.tyRef)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
    Lang.TyRef'LocalTyRef ltr ->
      [
        ( Just $ ltr ^. (Lang.tyName . Lang.sourceInfo)
        , "An unbound local type reference"
            <+> prettyTyRef (err ^. Compiler.tyRef)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
kindCheckErrors' (Compiler.KindCheckError'UnboundTyVarError' err) =
  [
    ( Just $ err ^. (Compiler.tyVar . Lang.varName . Lang.sourceInfo)
    , "An unbound type variable"
        <+> prettyName' (err ^. (Compiler.tyVar . Lang.varName))
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName' (err ^. (Compiler.tyDef . Lang.tyName))
    )
  ]
kindCheckErrors' (Compiler.KindCheckError'UnificationError' err) =
  [
    ( Just $ err ^. (Compiler.tyDef . Lang.sourceInfo)
    , "Unification error during kind checking"
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName' (err ^. (Compiler.tyDef . Lang.tyName))
          <> dot
        <+> "Tried to unify kind"
        <+> prettyKind (err ^. Compiler.tyKindLhs)
        <+> "with kind"
        <+> prettyKind (err ^. Compiler.tyKindRhs)
    )
  ]
kindCheckErrors' (Compiler.KindCheckError'CyclicKindError' err) =
  [
    ( Just $ err ^. (Compiler.tyDef . Lang.sourceInfo)
    , "Cyclic kind error (occurs check) during kind checking"
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName' (err ^. (Compiler.tyDef . Lang.tyName))
          <> dot
    )
  ]
kindCheckErrors' (Compiler.KindCheckError'InconsistentTypeError' err) =
  [
    ( Just $ err ^. (Compiler.tyDef . Lang.sourceInfo)
    , "Kind mismatch during kind checking"
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName' (err ^. (Compiler.tyDef . Lang.tyName))
          <> dot
        <+> "Expected kind"
        <+> prettyKind (err ^. Compiler.expectedKind)
        <+> "but got kind"
        <+> prettyKind (err ^. Compiler.actualKind)
    )
  ]

classCheckErrors' :: Compiler.TyClassCheckError'TyclassCheckError -> [(Maybe Lang.SourceInfo, Doc ann)]
classCheckErrors' (Compiler.TyClassCheckError'UnboundClassRefErr err) = case err ^. (Compiler.classRef . Lang.maybe'classRef) of
  Nothing ->
    [
      ( Just $ err ^. (Compiler.moduleName . Lang.sourceInfo)
      , "Unexpected error occurred while trying to report a `TyClassCheckError.UnboundClassRefErr` error. This likely means the Compiler responded with a malformed Compiler Output"
      )
    ]
  Just cr -> case cr of
    Lang.TyClassRef'ForeignClassRef fcr ->
      [
        ( Just $ fcr ^. (Lang.className . Lang.sourceInfo)
        , "An unbound foreign class reference"
            <+> prettyClassRef (err ^. Compiler.classRef)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
    Lang.TyClassRef'LocalClassRef lfr ->
      [
        ( Just $ lfr ^. (Lang.className . Lang.sourceInfo)
        , "An unbound local class reference"
            <+> prettyClassRef (err ^. Compiler.classRef)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
classCheckErrors' (Compiler.TyClassCheckError'SuperclassCycleErr err) =
  [
    ( Just $ err ^. (Compiler.className . Lang.sourceInfo)
    , "A superclass cycle"
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a class definition for"
        <+> prettyName' (err ^. Compiler.className)
          <> dot
        <+> "The cycle consists of"
        <+> encloseSep "" "" "->" (prettyClassRef <$> err ^. Compiler.cycledClassRefs)
    )
  ]
classCheckErrors' (Compiler.TyClassCheckError'ImportNotFoundErr err) =
  [
    ( Just $ err ^. (Compiler.moduleName . Lang.sourceInfo)
    , "A required module"
        <+> prettyModuleName' (err ^. Compiler.missing)
        <+> "is missing in the Compiler Input"
        <+> ", but is required by module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
    )
  ]
classCheckErrors' (Compiler.TyClassCheckError'DeriveOpaqueErr err) =
  [
    ( Just $ err ^. (Compiler.constraint . Lang.sourceInfo)
    , "When solving a constraint"
        <+> prettyConstraint (err ^. Compiler.constraint)
        <+> "a derive rule was found for an `opaque` type"
        <+> prettyConstraint (err ^. Compiler.subConstraint)
        <+> "in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
          <> dot
        <+> "`derive` statements can be applied only to 'transparent' types (ie. `sum`/`record`/`product`), use 'instance' statement instead to declare a rule on an `opaque` type'"
    )
  ]
classCheckErrors' (Compiler.TyClassCheckError'MissingRuleErr err) =
  [
    ( Just $ err ^. (Compiler.constraint . Lang.sourceInfo)
    , "When solving a constraint"
        <+> prettyConstraint (err ^. Compiler.constraint)
        <+> "no rule was found (`derive` or `instance`) to solve a goal"
        <+> prettyConstraint (err ^. Compiler.subConstraint)
        <+> "in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
    )
  ]
classCheckErrors' (Compiler.TyClassCheckError'OverlappingRulesErr err) =
  [
    ( Just $ err ^. (Compiler.constraint . Lang.sourceInfo)
    , "When solving a constraint"
        <+> prettyConstraint (err ^. Compiler.constraint)
        <+> "multiple rules were found (`derive` or `instance`) that could be used to solve a goal"
        <+> prettyConstraint (err ^. Compiler.subConstraint)
        <+> "in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
          <> dot
        <+> "The overlapping rules found are"
          <> line
        <+> vsep
          [ prettyConstraint (qhead ^. Compiler.head) <+> "defined in module" <+> prettyModuleName' (qhead ^. Compiler.moduleName)
          | qhead <- err ^. Compiler.overlaps
          ]
    )
  ]

namingCheckErrors' :: Compiler.NamingError'NamingError -> [(Maybe Lang.SourceInfo, Doc ann)]
namingCheckErrors' (Compiler.NamingError'ModuleNameErr mnPart) =
  [
    ( Just $ mnPart ^. Lang.sourceInfo
    , "Module name part" <+> prettyName' mnPart <+> "has an invalid format"
    )
  ]
namingCheckErrors' (Compiler.NamingError'TyNameErr tyN) =
  [
    ( Just $ tyN ^. Lang.sourceInfo
    , "Type name" <+> prettyName' tyN <+> "has an invalid format"
    )
  ]
namingCheckErrors' (Compiler.NamingError'VarNameErr varN) =
  [
    ( Just $ varN ^. Lang.sourceInfo
    , "Type variable name" <+> prettyName' varN <+> "has an invalid format"
    )
  ]
namingCheckErrors' (Compiler.NamingError'ConstrNameErr constrN) =
  [
    ( Just $ constrN ^. Lang.sourceInfo
    , "Sum constructor name" <+> prettyName' constrN <+> "has an invalid format"
    )
  ]
namingCheckErrors' (Compiler.NamingError'FieldNameErr fieldN) =
  [
    ( Just $ fieldN ^. Lang.sourceInfo
    , "Record field name" <+> prettyName' fieldN <+> "has an invalid format"
    )
  ]
namingCheckErrors' (Compiler.NamingError'ClassNameErr classN) =
  [
    ( Just $ classN ^. Lang.sourceInfo
    , "Type class name" <+> prettyName' classN <+> "has an invalid format"
    )
  ]

-- | Utils TODO(bladyjoker): Move to Utils module.
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

toSyntaxConstraint :: Lang.Constraint -> Either Text (Syntax.Constraint ())
toSyntaxConstraint langCstr = Syntax.Constraint <$> toSyntaxClassRef (langCstr ^. Lang.classRef) <*> toSyntaxTy `traverse` (langCstr ^. Lang.args) <*> pure ()

toSyntaxModuleAlias :: Lang.ModuleName -> Maybe (Syntax.ModuleAlias ())
toSyntaxModuleAlias mn = Just $ Syntax.ModuleAlias (toSyntaxModuleName mn) ()

toSyntaxModuleName :: Lang.ModuleName -> Syntax.ModuleName ()
toSyntaxModuleName mn = Syntax.ModuleName [Syntax.ModuleNamePart (p ^. Lang.name) () | p <- mn ^. Lang.parts] ()

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
