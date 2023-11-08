module LambdaBuffers.Codegen.Plutarch.Print.Syntax (filepathFromModuleName, printModName, cabalFromLbModuleName, fromLbForeignRef) where

import Control.Lens ((^.))
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty))

fromLbModuleName :: PC.ModuleName -> HsSyntax.ModuleName
fromLbModuleName mn = HsSyntax.MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".Plutarch"

cabalFromLbModuleName :: PC.ModuleName -> HsSyntax.CabalPackageName
cabalFromLbModuleName mn = HsSyntax.MkCabalPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-plutarch-lb"])

fromLbForeignRef :: PC.ForeignRef -> HsSyntax.QTyName
fromLbForeignRef fr =
  ( cabalFromLbModuleName $ fr ^. #moduleName
  , fromLbModuleName $ fr ^. #moduleName
  , HsSyntax.fromLbTyName $ fr ^. #tyName
  )

printModName :: PC.ModuleName -> Doc ann
printModName mn = let HsSyntax.MkModuleName hmn = fromLbModuleName mn in pretty hmn

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> "/Plutarch.hs"
