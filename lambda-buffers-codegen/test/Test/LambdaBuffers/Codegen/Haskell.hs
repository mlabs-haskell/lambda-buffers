module Test.LambdaBuffers.Codegen.Haskell (tests) where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Config (Config (MkConfig))
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.Types (ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), TyName (TyName))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

import LambdaBuffers.Codegen.Haskell.Config qualified as H
import LambdaBuffers.Codegen.Haskell.PrintM qualified as H
import LambdaBuffers.Compiler.ProtoCompat.FromProto qualified as PC
import Prettyprinter (vsep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Haskell"
    [testCase "should succeed" $ testPrint testCompInp testConfig ""]

testPrint :: P.CompilerInput -> H.Config -> String -> Assertion
testPrint compInp cfg want = do
  compInp' <- case PC.runFromProto compInp of
    Left err -> assertFailure (show err)
    Right res -> return res
  modDocs <-
    for
      (compInp' ^. #modules)
      ( \m -> case H.runPrint cfg m of
          Left err -> assertFailure (show err)
          Right res -> return res
      )
  print @String ""
  print $ vsep (toList modDocs)
  assertEqual "" want (show $ vsep (toList modDocs))

testCompInp :: P.CompilerInput
testCompInp =
  defMessage
    & P.modules
      .~ [testModule1, testModule2]

testConfig :: H.Config
testConfig =
  MkConfig
    ( Map.fromList
        [
          (
            ( PC.ModuleName [PC.ModuleNamePart "TestMod" PC.defSourceInfo] PC.defSourceInfo
            , PC.TyName "I8" PC.defSourceInfo
            )
          , (H.MkCabalPackageName "base", H.MkModuleName "Data.Int", H.MkTyName "Int8")
          )
        ,
          (
            ( PC.ModuleName [PC.ModuleNamePart "TestMod2" PC.defSourceInfo] PC.defSourceInfo
            , PC.TyName "I16" PC.defSourceInfo
            )
          , (H.MkCabalPackageName "base", H.MkModuleName "Data.Int", H.MkTyName "Int16")
          )
        ,
          (
            ( ModuleName [ModuleNamePart "TestMod" PC.defSourceInfo] PC.defSourceInfo
            , TyName "Set" PC.defSourceInfo
            )
          , (H.MkCabalPackageName "containers", H.MkModuleName "Data.Set", H.MkTyName "Set")
          )
        ]
    )
    mempty

testModule1 :: P.Module
testModule1 =
  defMessage
    & P.moduleName .~ mkModuleName ["TestMod"]
    & P.typeDefs
      .~ [ mkTyDefOpq "I8" []
         , mkTyDefOpq "Set" ["a"]
         , mkTyDefSum
            "Maybe"
            ["a"]
            ( mkSum
                [ ("Just", mkTuple [mkTyVar "a"])
                , ("Nothing", mkTuple [])
                ]
            )
         , mkTyDefSum
            "Either"
            ["a", "b"]
            ( mkSum
                [ ("Left", mkTuple [mkTyVar "a"])
                , ("Right", mkTuple [mkTyVar "b"])
                ]
            )
         ]
    & P.classDefs
      .~ [ defMessage
            & P.className . P.name .~ "Eq"
            & P.classArgs .~ [mkArg "a"]
         , defMessage
            & P.className . P.name .~ "Ord"
            & P.classArgs .~ [mkArg "a"]
         ]
    & P.instances
      .~ [ defMessage
            & P.classRef . P.localClassRef . P.className . P.name .~ "Eq"
            & P.args
              .~ [ defMessage
                    & P.tyApp . P.tyFunc . P.tyRef . P.localTyRef . P.tyName . P.name .~ "Maybe"
                    & P.tyApp . P.tyArgs .~ [mkTyVar "a"]
                 ]
         ]

testModule2 :: P.Module
testModule2 =
  defMessage
    & P.moduleName .~ mkModuleName ["TestMod2"]
    & P.typeDefs
      .~ [ mkTyDefOpq "I16" []
         , mkTyDefSum
            "Foo"
            ["a"]
            ( mkSum
                [ ("MkFoo", mkTuple [mkTyVar "a", mkLRef "I16", mkFRef ["TestMod"] "I8"])
                ]
            )
         ]

mkArg :: Text -> P.TyArg
mkArg vn =
  defMessage
    & P.argName . P.name .~ vn
    & P.argKind . P.kindRef .~ P.Kind'KIND_REF_TYPE

mkTyVar :: Text -> P.Ty
mkTyVar vn = defMessage & P.tyVar . P.varName . P.name .~ vn

mkFRef :: [Text] -> Text -> P.Ty
mkFRef mn tn =
  defMessage
    & P.tyRef . P.foreignTyRef . P.moduleName .~ mkModuleName mn
    & P.tyRef . P.foreignTyRef . P.tyName .~ mkTyName tn

mkLRef :: Text -> P.Ty
mkLRef tn =
  defMessage
    & P.tyRef . P.localTyRef . P.tyName .~ mkTyName tn

mkModuleName :: [Text] -> P.ModuleName
mkModuleName parts = defMessage & P.parts .~ [defMessage & P.name .~ p | p <- parts]

mkTyName :: Text -> P.TyName
mkTyName tn = defMessage & P.name .~ tn

mkTyDefOpq :: Text -> [Text] -> P.TyDef
mkTyDefOpq tn args =
  defMessage
    & P.tyName . P.name .~ tn
    & P.tyAbs . P.tyArgs .~ (mkArg <$> args)
    & P.tyAbs . P.tyBody . P.opaque .~ defMessage

mkTyDefSum :: Text -> [Text] -> P.Sum -> P.TyDef
mkTyDefSum tn args s =
  defMessage
    & P.tyName . P.name .~ tn
    & P.tyAbs . P.tyArgs .~ (mkArg <$> args)
    & P.tyAbs . P.tyBody . P.sum .~ s

mkSum :: [(Text, P.Product)] -> P.Sum
mkSum ctors =
  defMessage
    & P.constructors
      .~ [ defMessage
          & P.constrName . P.name .~ ctorN
          & P.product .~ p
         | (ctorN, p) <- ctors
         ]

mkTuple :: [P.Ty] -> P.Product
mkTuple tys = defMessage & P.ntuple . P.fields .~ tys
