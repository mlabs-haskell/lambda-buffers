module LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint) where

import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R

type MonadPrint m = Print.MonadPrint R.QTyName R.QClassName R.QValName m
