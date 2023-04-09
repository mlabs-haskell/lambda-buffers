module LambdaBuffers.Codegen.Haskell.Print.Monad (MonadPrint) where

import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print qualified as Print

type MonadPrint m = Print.MonadPrint H.QTyName H.QClassName H.QValName m
