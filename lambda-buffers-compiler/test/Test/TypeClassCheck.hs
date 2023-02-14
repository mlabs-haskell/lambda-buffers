module Test.TypeClassCheck (test) where

import Control.Lens ((.~), (^.))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat (runFromProto)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as ProtoCompat
import LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles')
import Proto.Compiler (ClassDef, CompilerInput, Constraint, Kind, Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler_Fields (argKind, argName, args, classArgs, classDefs, className, classRef, kindRef, localClassRef, moduleName, modules, name, parts, supers, tyVar, varName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Utils.Constructors (_ModuleName)

test :: TestTree
test =
  testGroup
    "TypeClassCheck tests"
    [ noCycleDetected
    , cycleDetected
    ]

noCycleDetected :: TestTree
noCycleDetected =
  testCase "No cycle detected" $ do
    nocycles' <- fromProto' nocycles
    case Map.lookup (_ModuleName ["ModuleWithNoClassCycles"]) (nocycles' ^. #modules) of
      Nothing -> assertFailure "Failed lookup to ModuleWithClassNoClassCycles"
      Just m -> detectSuperclassCycles' (toList $ m ^. #classDefs) @?= []

cycleDetected :: TestTree
cycleDetected =
  testCase "Cycle detected" $ do
    cycles' <- fromProto' cycles
    case Map.lookup (_ModuleName ["ModuleWithClassCycles"]) (cycles' ^. #modules) of
      Nothing -> assertFailure "Failed lookup to ModuleWithClassCycles"
      Just m -> detectSuperclassCycles' (toList $ m ^. #classDefs) @?= [["Bar", "Foo", "Bop", "Bar"], ["Bop", "Bar", "Foo", "Bop"], ["Foo", "Bop", "Bar", "Foo"]]

fromProto' :: CompilerInput -> IO ProtoCompat.CompilerInput
fromProto' compInp =
  either
    (\errs -> assertFailure $ "FromProto failed with " <> show errs)
    return
    (runFromProto compInp)

star :: Kind
star = defMessage & kindRef .~ Kind'KIND_REF_TYPE

mkclass :: Text -> [Text] -> ClassDef
mkclass nm sups =
  defMessage
    & className . name .~ nm
    & classArgs
      .~ [ defMessage
            & argName . name .~ "a"
            & argKind .~ star
         ]
    & supers .~ map constraint sups

constraint :: Text -> Constraint
constraint nm =
  defMessage
    & classRef . localClassRef . className . name .~ nm
    & args .~ [defMessage & tyVar . varName . name .~ "a"]

cycles :: CompilerInput
cycles =
  defMessage
    & modules
      .~ [ defMessage
            & moduleName . parts .~ [defMessage & name .~ "ModuleWithClassCycles"]
            & classDefs
              .~ [ mkclass "Foo" ["Bar", "Baz", "Beep"]
                 , mkclass "Bar" ["Bip", "Bop"]
                 , mkclass "Bop" ["Foo"]
                 ]
         ]

nocycles :: CompilerInput
nocycles =
  defMessage
    & modules
      .~ [ defMessage
            & moduleName . parts .~ [defMessage & name .~ "ModuleWithNoClassCycles"]
            & classDefs
              .~ [ mkclass "Functor" []
                 , mkclass "Applicative" ["Functor"]
                 , mkclass "Monad" ["Applicative"]
                 , mkclass "Traversable" ["Foldable", "Functor"]
                 ]
         ]
