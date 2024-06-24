module LambdaBuffers.Codegen.Haskell.Backend.PlutusTx (PlutusTxHaskellBackend) where

import Control.Lens ((^.))
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Backend (IsHaskellBackend (HaskellBackendContext, HaskellBackendState, filepathFromLbModuleName, fromLbModuleName, ghcOptions, languageExtensions, printImplementation, printModule, printTyDef))
import LambdaBuffers.Codegen.Haskell.Backend.PlutusTx.Derive qualified as PlutusTxHaskellBackend
import LambdaBuffers.Codegen.Haskell.Print qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as Haskell

data PlutusTxHaskellBackend

instance IsHaskellBackend PlutusTxHaskellBackend where
  type HaskellBackendContext PlutusTxHaskellBackend = ()
  type HaskellBackendState PlutusTxHaskellBackend = ()
  fromLbModuleName mn = Haskell.MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".PlutusTx"
  filepathFromLbModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> "/PlutusTx.hs"
  printImplementation = PlutusTxHaskellBackend.instancePrinters
  printModule = Haskell.printModule
  printTyDef = Haskell.printTyDef
  languageExtensions = ["NoImplicitPrelude", "NoPolyKinds"] -- NOTE(bladyjoker): NoPolyKinds is needed for PlutusTx compiler, quite safe.
  ghcOptions =
    [ "-fno-ignore-interface-pragmas" -- NOTE(bladyjoker): All this is necessary for PlutusTx compiler, apparently safe.
    , "-fno-omit-interface-pragmas"
    , "-fno-specialise"
    , "-fno-strictness"
    , "-fobject-code"
    ]
