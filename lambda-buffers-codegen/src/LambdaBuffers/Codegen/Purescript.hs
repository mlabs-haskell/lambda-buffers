module LambdaBuffers.Codegen.Purescript (
  runPrint,
) where

import Control.Lens ((^.))
import Data.Text (Text)
import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Config qualified as Purescript
import LambdaBuffers.Codegen.Purescript.Print qualified as Purescript
import LambdaBuffers.Codegen.Purescript.Syntax (filepathFromModuleName)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Compiler qualified as P

runPrint :: Purescript.Config -> PC.CompilerInput -> PC.Module -> Either P.CompilerError (FilePath, Text)
runPrint cfg ci m = case runCheck cfg ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint ctx Purescript.printModule of
    Left err -> Left err
    Right modDoc ->
      Right
        ( filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        )
