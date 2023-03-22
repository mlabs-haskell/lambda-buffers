module LambdaBuffers.Codegen.Purescript.Print.Monad (MonadPrint) where

import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs

type MonadPrint m = Print.MonadPrint Purs.QTyName [Purs.QClassName] m
