{- | Compiler error printing

Why is this here and not in the Compiler? It's because errors must be presented
in terms of the Frontend constructs. For instance, when printing out a type
expressions or a constraint. We're reusing the PPrint module available in the
Frontend to print out these concepts when they are needed in the error.

Another example of where this could be even better is if the reporting also had
a Frontend module context such that we can restore original module aliases with
type references where they are indeed used.
-}
module LambdaBuffers.Frontend.Errors.Compiler (showErrors) where

import Control.Lens ((^.))
import LambdaBuffers.Frontend.Errors.Utils (prettyClassRef, prettyConstraint, prettyKind, prettyModuleName', prettyName', prettySourceInfo, prettyTyRef)
import LambdaBuffers.Frontend.PPrint ()
import Prettyprinter (Doc, Pretty (pretty), defaultLayoutOptions, dot, encloseSep, layoutPretty, line, squotes, vsep, (<+>))
import Prettyprinter.Render.String (renderString)
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

showErrors :: Compiler.Error -> [String]
showErrors err =
  [ render $ case maySi of
    Nothing -> doc
    Just si -> prettySourceInfo si <+> doc
  | (maySi, doc) <- toErrors err
  ]
  where
    render :: Doc ann -> String
    render = renderString . layoutPretty defaultLayoutOptions

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
