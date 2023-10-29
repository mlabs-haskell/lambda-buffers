{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LambdaBuffers.Codegen.Plutarch.Syntax (filepathFromModuleName, module HsSyntax) where

import Control.Lens ((^.))
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.Syntax as HsSyntax hiding (filepathFromModuleName)
import LambdaBuffers.ProtoCompat qualified as PC

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack $ Text.intercalate "/" ("LambdaBuffers/Plutarch" : [p ^. #name | p <- mn ^. #parts]) <> ".hs"
