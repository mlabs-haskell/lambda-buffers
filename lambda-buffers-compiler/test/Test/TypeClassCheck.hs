module Test.TypeClassCheck (test) where

import Control.Lens ((.~))
import Data.Function ((&))
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Compiler.ProtoCompat (IsMessage (fromProto))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as ProtoCompat
import LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles')
import Proto.Compiler (ClassDef, Constraint, Kind, Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler_Fields (argKind, argName, arguments, classArgs, className, kindRef, name, supers, tyVar, varName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
    nocycles' <- classDefsFromProto nocycles
    detectSuperclassCycles' nocycles' @?= []

cycleDetected :: TestTree
cycleDetected =
  testCase "Cycle detected" $ do
    cycles' <- classDefsFromProto cycles
    detectSuperclassCycles' cycles' @?= [["Bar", "Foo", "Bop", "Bar"], ["Bop", "Bar", "Foo", "Bop"], ["Foo", "Bop", "Bar", "Foo"]]

classDefsFromProto :: [ClassDef] -> IO [ProtoCompat.ClassDef]
classDefsFromProto cds = for cds (either (\err -> assertFailure $ "FromProto failed with " <> show err) return . fromProto @ClassDef @ProtoCompat.ClassDef)

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
    & className . name .~ nm
    & arguments .~ [defMessage & tyVar . varName . name .~ "a"]

cycles :: [ClassDef]
cycles =
  [ mkclass "Foo" ["Bar", "Baz", "Beep"]
  , mkclass "Bar" ["Bip", "Bop"]
  , mkclass "Bop" ["Foo"]
  ]

nocycles :: [ClassDef]
nocycles =
  [ mkclass "Functor" []
  , mkclass "Applicative" ["Functor"]
  , mkclass "Monad" ["Applicative"]
  , mkclass "Traversable" ["Foldable", "Functor"]
  ]
