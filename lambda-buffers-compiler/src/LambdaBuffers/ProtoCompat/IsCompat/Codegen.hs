module LambdaBuffers.ProtoCompat.IsCompat.Codegen (codegenInputFromProto) where

import Control.Lens ((&), (.~), (^.))
import Data.ProtoLens (defMessage)
import LambdaBuffers.ProtoCompat.IsCompat.Compiler (compilerInputFromProto)
import LambdaBuffers.ProtoCompat.IsCompat.Lang ()
import LambdaBuffers.ProtoCompat.Types qualified as Compat
import Proto.Codegen qualified as Codegen
import Proto.Codegen_Fields qualified as Codegen
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler

-- | Just reuses the Compiler.Input processing for now.
codegenInputFromProto :: Codegen.Input -> Either Codegen.Error Compat.CodegenInput
codegenInputFromProto cgInp =
  let
    compInp = defMessage & Compiler.modules .~ cgInp ^. Codegen.modules :: Compiler.Input
   in
    case compilerInputFromProto compInp of
      Left (_err :: Compiler.Error) ->
        Left $
          defMessage
            & Codegen.internalErrors
              .~ [ defMessage
                    & Codegen.msg .~ "The Codegen Input proto is invalid (make sure to run the Compiler before hand)"
                 ]
      Right compInp' -> Right $ Compat.CodegenInput (compInp' ^. #modules)
