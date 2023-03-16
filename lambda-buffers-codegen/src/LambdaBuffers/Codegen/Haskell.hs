module LambdaBuffers.Codegen.Haskell (
  runPrint,
) where

import Control.Lens ((^.))
import Data.Text (Text)
import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Syntax (filepathFromModuleName)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Compiler qualified as P

runPrint :: Haskell.Config -> PC.Module -> Either P.CompilerError (FilePath, Text)
runPrint cfg m = case runCheck cfg m of
  Left err -> Left err
  Right ctx -> case Print.runPrint ctx Haskell.printModule of
    Left err -> Left err
    Right modDoc ->
      Right
        ( filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        )
