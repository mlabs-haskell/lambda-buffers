module LambdaBuffers.Codegen.Rust (
  runPrint,
) where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Config qualified as RsConfig
import LambdaBuffers.Codegen.Rust.Print qualified as RsPrint
import LambdaBuffers.Codegen.Rust.Print.Derive qualified as RsDerive
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as RsSyntax
import LambdaBuffers.Codegen.Rust.Print.TyDef qualified as RsPrint
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

{- | `runPrint cfg inp mod` prints a LambdaBuffers checked module `mod`, given its entire compilation closure in `inp` and Rust configuration file in `cfg`.
  It either errors with an API error message or succeeds with a module filepath, code and package dependencies.
-}
runPrint :: RsConfig.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runPrint cfg ci m = case runCheck cfg ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint ctx (RsPrint.printModule rsPrintModuleEnv) of
    Left err -> Left err
    Right (modDoc, deps) ->
      Right
        ( RsSyntax.filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        , deps
        )

rsPrintModuleEnv :: MonadPrint m => RsPrint.PrintModuleEnv m ann
rsPrintModuleEnv =
  RsPrint.PrintModuleEnv
    RsSyntax.printModName
    RsDerive.rsClassImplPrinters
    RsPrint.printTyDef
    ["no_implicit_prelude"]
