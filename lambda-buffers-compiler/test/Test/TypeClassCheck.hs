module Test.TypeClassCheck (test) where

import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "TypeClassCheck tests"
    []

{- [noCycleDetected
, cycleDetected ] -}

{-
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
-}
