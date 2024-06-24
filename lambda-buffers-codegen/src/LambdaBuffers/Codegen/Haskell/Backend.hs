{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Codegen.Haskell.Backend (runBackend, MonadHaskellBackend, HaskellBackendM, HaskellBackend, IsHaskellBackend (..)) where

import Control.Lens ((^.))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check qualified as Check
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Print (IsBackend)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

class IsBackend (HaskellBackend t) => IsHaskellBackend t where
  type HaskellBackendContext t
  type HaskellBackendState t
  fromLbModuleName :: PC.ModuleName -> H.ModuleName
  filepathFromLbModuleName :: PC.ModuleName -> FilePath
  printModule :: MonadHaskellBackend t m => m (Doc ann, Set Text)
  printImplementation :: forall m ann. MonadHaskellBackend t m => Map H.QClassName (PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann))
  printTyDef :: MonadHaskellBackend t m => PC.TyDef -> m (Doc ann)
  languageExtensions :: [Text]
  ghcOptions :: [Text]

data HaskellBackend t

instance IsBackend (HaskellBackend t) where
  type BackendContext (HaskellBackend t) = HaskellBackendContext t
  type BackendState (HaskellBackend t) = HaskellBackendState t
  type BackendQualifiedValueName (HaskellBackend t) = H.QValName
  type BackendQualifiedTyName (HaskellBackend t) = H.QTyName
  type BackendQualifiedClassName (HaskellBackend t) = H.QClassName

type MonadHaskellBackend t m = (IsHaskellBackend t, Print.MonadPrint (HaskellBackend t) m)
type HaskellBackendM t = Print.PrintM (HaskellBackend t)

runBackend :: forall t. IsHaskellBackend t => HaskellBackendContext t -> HaskellBackendState t -> Haskell.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runBackend hsBackendCtx hsBackendState cfg ci m = case Check.runCheck @(HaskellBackend t) cfg hsBackendCtx ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint hsBackendState ctx (printModule @t) of
    Left err -> Left err
    Right (modDoc, deps) ->
      Right
        ( filepathFromLbModuleName @t (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        , deps
        )
