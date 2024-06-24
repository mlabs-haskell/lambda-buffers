{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Codegen.Rust.Backend (MonadRustBackend, RustBackendM, RustBackend, RustBackendContext (..)) where

import Data.Text (Text)
import LambdaBuffers.Codegen.Print (IsBackend)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as Rust

data RustBackend

instance IsBackend RustBackend where
  type BackendContext RustBackend = RustBackendContext
  type BackendState RustBackend = ()
  type BackendQualifiedValueName RustBackend = Rust.QValName
  type BackendQualifiedTyName RustBackend = Rust.QTyName
  type BackendQualifiedClassName RustBackend = Rust.QTraitName

type MonadRustBackend m = (Print.MonadPrint RustBackend m)
type RustBackendM = Print.PrintM RustBackend

data RustBackendContext = RustBackendContext
  { rust'compilationCfgs :: ![Text]
  , rust'packages :: !Rust.PkgMap
  }
