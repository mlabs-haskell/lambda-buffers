{-# LANGUAGE PatternSynonyms #-}

module Test.TypeClassCheck (test) where

import Control.Lens ((.~))
import Data.Function ((&))
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Compiler.ProtoCompat (IsMessage (fromProto))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as ProtoCompat
import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (
    AppP,
    DecP,
    Name,
    Nil,
    ProdP,
    RecP,
    RefP,
    SumP,
    VarP,
    (:*),
    (:=)
  ),
 )
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )
import LambdaBuffers.Compiler.TypeClass.Rules qualified as R
import LambdaBuffers.Compiler.TypeClass.Solve (solve)
import LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles')
import Proto.Compiler (ClassDef, Constraint, Kind, Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler_Fields (argKind, argName, arguments, classArgs, className, classRef, kindRef, localClassRef, name, supers, tyVar, varName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

test :: TestTree
test =
  testGroup
    "TypeClass tests"
    [cycleTests, solveTests]

cycleTests :: TestTree
cycleTests =
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

constraint :: Text -> Proto.Compiler.Constraint
constraint nm =
  defMessage
    & classRef . localClassRef . className . name .~ nm
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

---- Solver tests

solveTests :: TestTree
solveTests =
  testGroup
    "Solver tests"
    [ testCase "1: C [Maybe Int] (completeRules)" $
        solveTest1 @?= []
    , testCase "2: D [Maybe Int] (partialRules)" $
        solveTest2 @?= [dInt]
    , testCase "3: D [Maybe Int] (complete D, partial C)" $
        solveTest3 @?= [cInt]
    , testCase "4: C [[[Bool]]] (completeRules)" $
        solveTest4 @?= []
    , testCase "5: C (Either (Either Int Bool) (Either Bool Int)) (completeRules)" $
        solveTest5 @?= []
    , testCase "6: C (Either l x) (completeRules)" $
        solveTest6 @?= [cL, cX]
    , testCase "7: Sum test (completeRules)" $
        solveTest7 @?= []
    , testCase "8: Sum test (partialRules)" $
        solveTest8 @?= [cBool, cInt]
    , testCase "9: Rec test (completeRules)" $
        solveTest9 @?= []
    , testCase "10: Rec test (partialRules)" $
        solveTest10 @?= [cBool, cInt]
    , testCase "11: Prod test (completeRules)" $
        solveTest11 @?= []
    , testCase "12: Prod test (partialRules)" $
        solveTest12 @?= [cBool, cInt]
    , testCase "13: Dec test (completeRules)" $
        solveTest13 @?= []
    , testCase "14: Dec test (partialRules)" $
        solveTest14 @?= [cBool, cInt]
    ]
  where
    cInt = C _c Int
    cBool = C _c Bool
    cL = C _c _l
    cX = C _c _x
    dInt = C _d Int

-- hardcoded variables, easier to read than (VarP "blah") everywhere
_x, _xs, _vars, _name, _l, _body :: Pat
_x = VarP "x"
_xs = VarP "xs"
_vars = VarP "vars"
_name = VarP "name"
_l = VarP "l"
_body = VarP "body"

-- TODO(@gnumonik): Import this from somewhere else when that somewhere else module exists
mkStructuralRules :: Class -> [Rule]
mkStructuralRules c =
  [ C c Nil :<= []
  , C c (_x :* _xs) :<= [C c _x, C c _xs]
  , C c (_l := _x) :<= [C c _x]
  , C c (RecP _xs) :<= [C c _xs]
  , C c (ProdP _xs) :<= [C c _xs]
  , C c (SumP _xs) :<= [C c _xs]
  , C c (DecP _name _vars _body) :<= [C c _body]
  ]

pattern (:@) :: Pat -> Pat -> Pat
pattern (:@) p1 p2 = AppP p1 p2

pattern LocalRef :: Text -> Pat
pattern LocalRef nm = RefP Nil (Name nm)

pattern Maybe :: Pat -> Pat
pattern Maybe p1 = LocalRef "Maybe" :@ p1

pattern List :: Pat -> Pat
pattern List p = LocalRef "List" :@ p

pattern Either :: Pat -> Pat -> Pat
pattern Either l r = (LocalRef "Either" :@ l) :@ r

pattern Int :: Pat
pattern Int = LocalRef "Int"

pattern Bool :: Pat
pattern Bool = LocalRef "Bool"

pattern NoConstraints :: Class -> Pat -> Rule
pattern NoConstraints c p = C c p :<= []

pattern Labeled :: Text -> Pat -> Pat
pattern Labeled nm p = Name nm := p

userRules1 :: Class -> [Rule]
userRules1 c =
  [ NoConstraints c Int
  , NoConstraints c Bool
  , C c (Maybe _x) :<= [C c _x]
  , C c (Either _l _x) :<= [C c _l, C c _x]
  , C c (List _x) :<= [C c _x]
  ]

userRules2 :: Class -> [Rule]
userRules2 c =
  [ C c (Maybe _x) :<= [C c _x]
  , C c (Either _l _x) :<= [C c _l, C c _x]
  , C c (List _x) :<= [C c _x]
  ]

completeRules :: Class -> [Rule]
completeRules c = mkStructuralRules c <> userRules1 c

partialRules :: Class -> [Rule]
partialRules c = mkStructuralRules c <> userRules2 c

-- No supers
_c :: Class
_c = Class (FQClassName "C" []) []

-- C is super
_d :: Class
_d = Class (FQClassName "D" []) [_c]

-- C [Maybe Int] w/ complete rules (expected: [])
solveTest1 :: [R.Constraint]
solveTest1 = solve (completeRules _c) (C _c $ List (Maybe Int))

-- D [Maybe Int] w/ incomplete rules (expected: [D Int])
solveTest2 :: [R.Constraint]
solveTest2 = solve (partialRules _d) (C _d $ List (Maybe Int))

-- D [Maybe Int] w/ complete D rules & incomplete C rules (expected: [C Int])
solveTest3 :: [R.Constraint]
solveTest3 = solve rules (C _d $ List (Maybe Int))
  where
    rules = completeRules _d <> partialRules _c

-- C [[[Bool]]] w/ complete rules (expected: [])
solveTest4 :: [R.Constraint]
solveTest4 = solve (completeRules _c) $ C _c $ List (List (List Bool))

-- C (Either (Either Int Bool) (Either Bool Int)) w/ complete rules (expected: [])
solveTest5 :: [R.Constraint]
solveTest5 = solve (completeRules _c) $ C _c $ Either (Either Int Bool) (Either Bool Int)

-- NOTE(@bladyjoker): This one shows that we never need to "freshen" variables
--                    (if you study it you might also see why we never need to bind them)
-- C (Either l x) w/ complete rules (expected: [C l, C x])
solveTest6 :: [R.Constraint]
solveTest6 = solve (completeRules _c) $ C _c $ Either _l _x

-- tests for structural subcomponents of types. Can't write Haskell equivalents (w/o row-types)

-- expected: []
solveTest7 :: [R.Constraint]
solveTest7 = solve (completeRules _c) $ C _c sumBody
  where
    sumBody = SumP $ Labeled "Ctor1" Int :* Labeled "Ctor2" (List Bool) :* Nil

-- expected [C Bool, C Int]
solveTest8 :: [R.Constraint]
solveTest8 = solve (partialRules _c) $ C _c sumBody
  where
    sumBody = SumP $ Labeled "Ctor1" Int :* Labeled "Ctor2" (List Bool) :* Nil

-- expected []
solveTest9 :: [R.Constraint]
solveTest9 = solve (completeRules _c) $ C _c recBody
  where
    recBody =
      RecP $
        Labeled "field1" (Maybe Bool)
          :* Labeled "field2" (Either Int (List Int))
          :* Nil

-- expected [C Bool, C Int]
solveTest10 :: [R.Constraint]
solveTest10 = solve (partialRules _c) $ C _c recBody
  where
    recBody =
      RecP $
        Labeled "field1" (Maybe Bool)
          :* Labeled "field2" (Either Int (List Int))
          :* Nil

-- expected []
solveTest11 :: [R.Constraint]
solveTest11 = solve (completeRules _c) $ C _c prodBody
  where
    prodBody = ProdP $ List Bool :* Either Int Bool :* Nil

-- expected [C Bool, C Int]
solveTest12 :: [R.Constraint]
solveTest12 = solve (partialRules _c) $ C _c prodBody
  where
    prodBody = ProdP $ List Bool :* Either Int Bool :* Nil

-- expected []
solveTest13 :: [R.Constraint]
solveTest13 = solve (completeRules _c) $ C _c testDec
  where
    testDec =
      DecP (Name "Foo") Nil $
        SumP $
          Labeled "Ctor1" (ProdP $ Maybe Int :* Nil)
            :* Labeled "Ctor2" (RecP $ Labeled "field1" Bool :* Nil)
            :* Nil

-- expected [C Int, C Bool]
solveTest14 :: [R.Constraint]
solveTest14 = solve (partialRules _c) $ C _c testDec
  where
    testDec =
      DecP (Name "Foo") Nil $
        SumP $
          Labeled "Ctor1" (ProdP $ Maybe Int :* Nil)
            :* Labeled "Ctor2" (RecP $ Labeled "field1" Bool :* Nil)
            :* Nil
