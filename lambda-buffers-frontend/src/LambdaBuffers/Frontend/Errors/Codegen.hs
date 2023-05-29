module LambdaBuffers.Frontend.Errors.Codegen (showErrors) where

import Control.Lens ((^.))
import LambdaBuffers.Frontend.Errors.Utils (prettyModuleName', prettyName', prettySourceInfo)
import LambdaBuffers.Frontend.PPrint ()
import Prettyprinter (Doc, Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.String (renderString)
import Proto.Codegen qualified as Codegen
import Proto.Codegen_Fields qualified as Codegen
import Proto.Lang qualified as Lang

showErrors :: Codegen.Error -> [String]
showErrors err =
  [ render $ case maySi of
    Nothing -> doc
    Just si -> prettySourceInfo si <+> doc
  | (maySi, doc) <- toErrors err
  ]
  where
    render :: Doc ann -> String
    render = renderString . layoutPretty defaultLayoutOptions

toErrors :: Codegen.Error -> [(Maybe Lang.SourceInfo, Doc ann)]
toErrors err =
  internalErrors (err ^. Codegen.internalErrors)
    <> unsupportedOpaqueErrors (err ^. Codegen.unsupportedOpaqueErrors)
    <> unsupportedClassErrors (err ^. Codegen.unsupportedClassErrors)

unsupportedClassErrors :: [Codegen.UnsupportedClassError] -> [(Maybe Lang.SourceInfo, Doc ann)]
unsupportedClassErrors errs = do
  err <- errs
  return
    ( Nothing
    , "The requested Codegen backend doesn't support the LambdaBuffers type class" <+> prettyName' (err ^. Codegen.className) <+> "defined in module" <+> prettyModuleName' (err ^. Codegen.moduleName)
    )

unsupportedOpaqueErrors :: [Codegen.UnsupportedOpaqueError] -> [(Maybe Lang.SourceInfo, Doc ann)]
unsupportedOpaqueErrors errs = do
  err <- errs
  return
    ( Nothing
    , "The requested Codegen backend doesn't support the LambdaBuffers `opaque` type" <+> prettyName' (err ^. Codegen.tyName) <+> "defined in module" <+> prettyModuleName' (err ^. Codegen.moduleName)
    )

internalErrors :: [Codegen.InternalError] -> [(Maybe Lang.SourceInfo, Doc ann)]
internalErrors errs = do
  err <- errs
  return (Just $ err ^. Codegen.sourceInfo, "Codegen reported an internal error:" <+> pretty (err ^. Codegen.msg))
