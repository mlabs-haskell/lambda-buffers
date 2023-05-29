module LambdaBuffers.Compiler (runCompiler) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.KindCheck qualified as KindCheck
import LambdaBuffers.Compiler.TypeClassCheck qualified as TyClassCheck
import LambdaBuffers.ProtoCompat (
  compilerInputFromProto,
  toProto,
 )
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

runCompiler :: P.Input -> P.Output
runCompiler compInp = do
  case compilerInputFromProto compInp of
    Left err -> defMessage & P.error .~ err
    Right compInp' -> case KindCheck.runCheck compInp' of
      Left err -> defMessage & P.error .~ toProto err
      Right _ -> case TyClassCheck.runCheck compInp' of
        Left err -> defMessage & P.error .~ err
        Right _ -> defMessage & P.result .~ defMessage
