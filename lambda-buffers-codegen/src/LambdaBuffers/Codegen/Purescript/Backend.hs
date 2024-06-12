{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Codegen.Purescript.Backend (MonadPurescriptBackend, PurescriptBackendM, PurescriptBackend) where

import LambdaBuffers.Codegen.Print (IsBackend)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purescript

data PurescriptBackend

instance IsBackend PurescriptBackend where
  type BackendContext PurescriptBackend = ()
  type BackendState PurescriptBackend = ()
  type BackendQualifiedValueName PurescriptBackend = Purescript.QValName
  type BackendQualifiedTyName PurescriptBackend = Purescript.QTyName
  type BackendQualifiedClassName PurescriptBackend = Purescript.QClassName

type MonadPurescriptBackend m = (Print.MonadPrint PurescriptBackend m)
type PurescriptBackendM = Print.PrintM PurescriptBackend
