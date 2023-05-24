module LambdaBuffers.ProtoCompat.IsCompat (
  FromProto.toProto,
  Compiler.compilerInputFromProto,
) where

import LambdaBuffers.ProtoCompat.IsCompat.Compiler ()
import LambdaBuffers.ProtoCompat.IsCompat.Compiler qualified as Compiler
import LambdaBuffers.ProtoCompat.IsCompat.FromProto qualified as FromProto
import LambdaBuffers.ProtoCompat.IsCompat.Lang ()
