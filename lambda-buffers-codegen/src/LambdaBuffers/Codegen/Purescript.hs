module LambdaBuffers.Codegen.Purescript (
  runBackend,
) where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check qualified as Check
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Backend (PurescriptBackend)
import LambdaBuffers.Codegen.Purescript.Config qualified as Purescript
import LambdaBuffers.Codegen.Purescript.Print qualified as Purescript
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purescript
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

runBackend :: Purescript.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runBackend cfg ci m = case Check.runCheck @PurescriptBackend cfg () ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint () ctx Purescript.printModule of
    Left err -> Left err
    Right (modDoc, deps) ->
      Right
        ( Purescript.filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        , deps
        )
