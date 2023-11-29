module LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint) where

import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts

type MonadPrint m = Print.MonadPrint Ts.QTyName Ts.QClassName Ts.QValName m
