module LambdaBuffers.Compiler.Errors (toErrors) where

import Control.Lens ((^.))
import Data.ProtoLens.Field (HasField)
import LambdaBuffers.ProtoCompat.Utils (prettyModuleName')
import Prettyprinter (Doc, Pretty (pretty), colon, dot, parens, squotes, (<+>))
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

toErrors :: Compiler.Error -> [(Maybe Lang.SourceInfo, Doc ann)]
toErrors err =
  protoParseErrors (err ^. Compiler.protoParseErrors)
    <> internalErrors (err ^. Compiler.internalErrors)
    <> kindCheckErrors (err ^. Compiler.kindCheckErrors)

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

protoParseErrors' :: Compiler.ProtoParseError'ProtoParseError -> [(Maybe Lang.SourceInfo, Doc ann)]
protoParseErrors' (Compiler.ProtoParseError'MultipleClassdefError err) = do
  let mn = err ^. Compiler.moduleName
  cd <- err ^. Compiler.classDefs
  return
    ( Just $ cd ^. Lang.className . Lang.sourceInfo
    , "A class definition with a duplicate name" <+> prettyName (cd ^. Lang.className) <+> "was defined in module" <+> prettyModuleName' mn
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleTydefError err) = do
  let mn = err ^. Compiler.moduleName
  td <- err ^. Compiler.tyDefs
  return
    ( Just $ td ^. Lang.tyName . Lang.sourceInfo
    , "A type definition with a duplicate name" <+> prettyName (td ^. Lang.tyName) <+> "was defined in module" <+> prettyModuleName' mn
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
        <+> prettyName (arg ^. Lang.argName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName (td ^. Lang.tyName)
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleConstructorError' err) = do
  let mn = err ^. Compiler.moduleName
      td = err ^. Compiler.tyDef
  ctor <- err ^. Compiler.constructors
  return
    ( Just $ ctor ^. Lang.constrName . Lang.sourceInfo
    , "A sum type constructor definitions with a duplicate name"
        <+> prettyName (ctor ^. Lang.constrName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName (td ^. Lang.tyName)
    )
protoParseErrors' (Compiler.ProtoParseError'MultipleFieldError' err) = do
  let mn = err ^. Compiler.moduleName
      td = err ^. Compiler.tyDef
  field <- err ^. Compiler.fields
  return
    ( Just $ field ^. Lang.fieldName . Lang.sourceInfo
    , "A record type field definitions with a duplicate name"
        <+> prettyName (field ^. Lang.fieldName)
        <+> "was defined in module"
        <+> prettyModuleName' mn
        <+> "in a type definition for"
        <+> prettyName (td ^. Lang.tyName)
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
      , "Unexpected error occurred while trying to report a `KindCheckError'UnboundTyRefError` error. This likely means the Compiler responded with a malformed Compiler Output"
      )
    ]
  Just tr -> case tr of
    Lang.TyRef'ForeignTyRef ftr ->
      [
        ( Just $ ftr ^. (Lang.tyName . Lang.sourceInfo)
        , "An unbound foreign type reference"
            <+> prettyModuleName' (ftr ^. Lang.moduleName) <> colon <> prettyName (ftr ^. Lang.tyName)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
    Lang.TyRef'LocalTyRef ltr ->
      [
        ( Just $ ltr ^. (Lang.tyName . Lang.sourceInfo)
        , "An unbound local type reference"
            <+> prettyName (ltr ^. Lang.tyName)
            <+> "was found in module"
            <+> prettyModuleName' (err ^. Compiler.moduleName)
        )
      ]
kindCheckErrors' (Compiler.KindCheckError'UnboundTyVarError' err) =
  [
    ( Just $ err ^. (Compiler.tyVar . Lang.varName . Lang.sourceInfo)
    , "An unbound type variable"
        <+> prettyName (err ^. (Compiler.tyVar . Lang.varName))
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName (err ^. (Compiler.tyDef . Lang.tyName))
    )
  ]
kindCheckErrors' (Compiler.KindCheckError'UnificationError' err) =
  [
    ( Just $ err ^. (Compiler.tyDef . Lang.sourceInfo)
    , "Unification error during kind checking"
        <+> "was found in module"
        <+> prettyModuleName' (err ^. Compiler.moduleName)
        <+> "in a type definition for"
        <+> prettyName (err ^. (Compiler.tyDef . Lang.tyName))
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
        <+> prettyName (err ^. (Compiler.tyDef . Lang.tyName))
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
        <+> prettyName (err ^. (Compiler.tyDef . Lang.tyName))
          <> dot
        <+> "Expected kind"
        <+> prettyKind (err ^. Compiler.expectedKind)
        <+> "but got kind"
        <+> prettyKind (err ^. Compiler.actualKind)
    )
  ]

prettyKind :: Lang.Kind -> Doc ann
prettyKind k = case k ^. Lang.maybe'kind of
  Nothing -> "TODO(bladyjoker): Error while printing Kind"
  Just k' -> case k' of
    Lang.Kind'KindRef kr -> case kr of
      Lang.Kind'KIND_REF_TYPE -> "*"
      _ -> "<TODO(bladyjoker): Something went wrong when printing a KindRef>"
    Lang.Kind'KindArrow' ka -> parens (prettyKind (ka ^. Lang.left) <+> "->" <+> prettyKind (ka ^. Lang.right))

prettyName :: forall {a} {s} {ann}. (Pretty a, HasField s "name" a) => s -> Doc ann
prettyName x = squotes (pretty (x ^. Lang.name))
