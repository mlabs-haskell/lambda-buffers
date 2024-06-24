module LambdaBuffers.Codegen.Haskell.Backend.Plutarch (PlutarchHaskellBackend) where

import Control.Lens ((^.))
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Backend (IsHaskellBackend (HaskellBackendContext, HaskellBackendState, filepathFromLbModuleName, fromLbModuleName, ghcOptions, languageExtensions, printImplementation, printModule, printTyDef))
import LambdaBuffers.Codegen.Haskell.Backend.Plutarch.Derive qualified as PlutarchHaskellBackend
import LambdaBuffers.Codegen.Haskell.Backend.Plutarch.TyDef qualified as PlutarchHaskellBackend

import LambdaBuffers.Codegen.Haskell.Print qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell

data PlutarchHaskellBackend

instance IsHaskellBackend PlutarchHaskellBackend where
  type HaskellBackendContext PlutarchHaskellBackend = ()
  type HaskellBackendState PlutarchHaskellBackend = ()
  fromLbModuleName mn = Haskell.MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> ".Plutarch"
  filepathFromLbModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts]) <> "/Plutarch.hs"
  printImplementation = PlutarchHaskellBackend.hsClassImplPrinters
  printModule = Haskell.printModule
  printTyDef = PlutarchHaskellBackend.printTyDef
  languageExtensions =
    [ "KindSignatures"
    , "DataKinds"
    , "TypeFamilies"
    , "MultiParamTypeClasses"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "DerivingStrategies"
    , "DeriveAnyClass"
    , "DeriveGeneric"
    , "UndecidableInstances"
    ]
  ghcOptions = []
