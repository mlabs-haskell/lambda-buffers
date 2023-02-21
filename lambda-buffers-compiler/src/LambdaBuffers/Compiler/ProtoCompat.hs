{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LambdaBuffers.Compiler.ProtoCompat (
  module FromProto,
  module Types,
  module SILEq,
) where

import LambdaBuffers.Compiler.ProtoCompat.FromProto as FromProto
import LambdaBuffers.Compiler.ProtoCompat.SILEq as SILEq
import LambdaBuffers.Compiler.ProtoCompat.Types as Types
