module LambdaBuffers.Codegen.Rust (
  runBackend,
) where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check qualified as Check
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Backend (RustBackend, RustBackendContext (RustBackendContext, rust'compilationCfgs, rust'packages))
import LambdaBuffers.Codegen.Rust.Config qualified as Rust
import LambdaBuffers.Codegen.Rust.Print qualified as Rust
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as Rust
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (
  defaultLayoutOptions,
  layoutPretty,
 )
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

runBackend :: Rust.PkgMap -> Rust.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runBackend pkgs cfg ci m =
  let
    rustCtx =
      RustBackendContext
        { rust'packages = pkgs
        , rust'compilationCfgs = ["no_implicit_prelude", "allow(warnings)"]
        }
   in
    case Check.runCheck @RustBackend cfg rustCtx ci m of
      Left err -> Left err
      Right ctx -> case Print.runPrint () ctx Rust.printModule of
        Left err -> Left err
        Right (modDoc, deps) ->
          Right
            ( Rust.filepathFromModuleName (m ^. #moduleName)
            , renderStrict $ layoutPretty defaultLayoutOptions modDoc
            , deps
            )
