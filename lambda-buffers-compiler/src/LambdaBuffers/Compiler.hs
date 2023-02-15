module LambdaBuffers.Compiler (runCompiler) where

import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.KindCheck (check_)
import LambdaBuffers.Compiler.ProtoCompat.FromProto (
  runFromProto,
  toProto,
 )
import Proto.Compiler (CompilerError, CompilerInput, CompilerResult)

runCompiler :: CompilerInput -> Either CompilerError CompilerResult
runCompiler compInp = do
  compInp' <- runFromProto compInp
  case check_ compInp' of
    Left err -> Left $ toProto err
    Right _ -> Right defMessage
