module LambdaBuffers.Compiler (runCompiler) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.KindCheck qualified as KindCheck
import LambdaBuffers.Compiler.ProtoCompat.FromProto (
  runFromProto,
  toProto,
 )
import Proto.Compiler (CompilerInput, CompilerOutput)
import Proto.Compiler_Fields qualified as P

runCompiler :: CompilerInput -> CompilerOutput
runCompiler compInp = do
  case runFromProto compInp of
    Left err -> defMessage & P.compilerError .~ err
    Right compInp' -> toProto $ KindCheck.check compInp'
