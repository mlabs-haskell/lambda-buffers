module LambdaBuffers.Codegen.Haskell (
  runPrint,
) where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.Haskell.Config qualified as HsConfig
import LambdaBuffers.Codegen.Haskell.Print qualified as HsPrint
import LambdaBuffers.Codegen.Haskell.Print.Derive qualified as HsDerive
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as HsPrint
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

{- | `runPrint cfg inp mod` prints a LambdaBuffers checked module `mod`, given its entire compilation closure in `inp` and Haskell configuration file in `cfg`.
  It either errors with an API error message or succeeds with a module filepath, code and package dependencies.
-}
runPrint :: HsConfig.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runPrint cfg ci m = case runCheck cfg ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint ctx (HsPrint.printModule hsPrintModuleEnv) of
    Left err -> Left err
    Right (modDoc, deps) ->
      Right
        ( HsSyntax.filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        , deps
        )

hsPrintModuleEnv :: HsPrint.PrintModuleEnv m ann
hsPrintModuleEnv =
  HsPrint.PrintModuleEnv
    HsSyntax.printModName
    HsDerive.hsClassImplPrinters
    HsPrint.printTyDef
