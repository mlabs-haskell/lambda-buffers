module LambdaBuffers.Codegen.Typescript (runBackend) where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Check qualified as Check
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Backend (TypescriptBackend)
import LambdaBuffers.Codegen.Typescript.Config qualified as Typescript
import LambdaBuffers.Codegen.Typescript.Print qualified as Typescript
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Typescript
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

runBackend :: Typescript.Config -> Typescript.PkgMap -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runBackend cfg pkgMap ci m = case Check.runCheck @TypescriptBackend cfg () ci m of
  Left err -> Left err
  Right ctx -> case Print.runPrint () ctx (Typescript.printModule pkgMap) of
    Left err -> Left err
    Right (modDoc, deps) ->
      Right
        ( Typescript.filepathFromModuleName (m ^. #moduleName)
        , renderStrict $ layoutPretty defaultLayoutOptions modDoc
        , deps
        )
