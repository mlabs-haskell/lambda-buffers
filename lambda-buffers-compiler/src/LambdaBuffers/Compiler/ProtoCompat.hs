{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LambdaBuffers.Compiler.ProtoCompat (
  module FromProto,
  module Types,
  module InfoLess,
) where

import LambdaBuffers.Compiler.ProtoCompat.FromProto as FromProto
import LambdaBuffers.Compiler.ProtoCompat.InfoLess as InfoLess
import LambdaBuffers.Compiler.ProtoCompat.Types as Types
