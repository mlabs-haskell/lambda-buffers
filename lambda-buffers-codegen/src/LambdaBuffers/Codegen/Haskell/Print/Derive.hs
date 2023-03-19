module LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEq) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.LamVal (printImplementation)
import LambdaBuffers.Codegen.Haskell.Print.Names (printHsValName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, equals, vsep, (<+>))

lvBuiltins :: Map LV.ValueName (H.CabalPackageName, H.ModuleName, H.ValueName)
lvBuiltins =
  Map.fromList
    [ ("eq", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "=="))
    , ("and", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "&&"))
    , ("true", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "True"))
    , ("false", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "False"))
    ]

eqClassMethodName :: H.ValueName
eqClassMethodName = H.MkValueName "=="

-- TODO: Handle errors properly.
printDeriveEq :: PC.ModuleName -> PC.TyDefs -> Map H.QClassName (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set H.QValName)
printDeriveEq mn iTyDefs mkInstanceDocs ty =
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let instanceDocs = ($ printValueDef eqClassMethodName implDoc) <$> mkInstanceDocs
           in Right (vsep (toList instanceDocs), Set.fromList . toList $ lvBuiltins)

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printHsValName valName <+> equals <+> valDoc
