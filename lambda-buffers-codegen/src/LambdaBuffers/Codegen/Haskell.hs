module LambdaBuffers.Codegen.Haskell (
  runPrint,
) where

import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print qualified as Haskell
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc)
import Proto.Compiler qualified as P

runPrint :: Haskell.Config -> PC.Module -> Either P.CompilerError (Doc ())
runPrint cfg m = case runCheck cfg m of
  Left err -> Left err
  Right ctx -> Print.runPrint ctx Haskell.printModule
