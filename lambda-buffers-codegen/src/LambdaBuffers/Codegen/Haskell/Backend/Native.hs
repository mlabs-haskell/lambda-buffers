module LambdaBuffers.Codegen.Haskell.Backend.Native (NativeHaskellBackend) where

import Control.Lens ((^.))
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Backend (IsHaskellBackend (HaskellBackendContext, HaskellBackendState, filepathFromLbModuleName, fromLbModuleName, ghcOptions, languageExtensions, printImplementation, printModule, printTyDef))
import LambdaBuffers.Codegen.Haskell.Backend.Native.Derive qualified as NativeHaskellBackend
import LambdaBuffers.Codegen.Haskell.Print qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as Haskell

data NativeHaskellBackend

instance IsHaskellBackend NativeHaskellBackend where
  type HaskellBackendContext NativeHaskellBackend = ()
  type HaskellBackendState NativeHaskellBackend = ()
  fromLbModuleName mn = Haskell.MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".PlutusTx"
  filepathFromLbModuleName mn = Text.unpack (Text.replace "." "/" (let Haskell.MkModuleName txt = fromLbModuleName @NativeHaskellBackend mn in txt)) <> ".hs"
  printImplementation = NativeHaskellBackend.hsClassImplPrinters
  printModule = Haskell.printModule
  printTyDef = Haskell.printTyDef
  languageExtensions = []
  ghcOptions = []
