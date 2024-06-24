{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Codegen.Typescript.Backend (MonadTypescriptBackend, TypescriptBackendM, TypescriptBackend) where

import LambdaBuffers.Codegen.Print (IsBackend)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Typescript

data TypescriptBackend

instance IsBackend TypescriptBackend where
  type BackendContext TypescriptBackend = ()
  type BackendState TypescriptBackend = ()
  type BackendQualifiedValueName TypescriptBackend = Typescript.QValName
  type BackendQualifiedTyName TypescriptBackend = Typescript.QTyName
  type BackendQualifiedClassName TypescriptBackend = Typescript.QClassName

type MonadTypescriptBackend m = (Print.MonadPrint TypescriptBackend m)
type TypescriptBackendM = Print.PrintM TypescriptBackend
