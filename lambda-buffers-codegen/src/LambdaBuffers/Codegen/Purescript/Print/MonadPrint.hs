module LambdaBuffers.Codegen.Purescript.Print.MonadPrint (MonadPrint) where

import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs

type MonadPrint m = Print.MonadPrint Purs.QTyName Purs.QClassName Purs.QValName m
