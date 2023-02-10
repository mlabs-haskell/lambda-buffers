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
    TyVarP,
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
import LambdaBuffers.Compiler.TypeClass.Solve (Overlap (Overlap), solve)
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
        solveTest1 @?= solved
    , testCase "2: D [Maybe Int] (partialRules)" $
        solveTest2 @?= Right [cListMaybeInt, cMaybeInt, dInt]
    , testCase "3: D [Maybe Int] (complete D, partial C)" $
        solveTest3 @?= Right [cInt]
    , testCase "4: C [[[Bool]]] (completeRules)" $
        solveTest4 @?= solved
    , testCase "5: C (Either (Either Int Bool) (Either Bool Int)) (completeRules)" $
        solveTest5 @?= solved
    , testCase "6: C (Either l x) (completeRules)" $
        solveTest6 @?= Right [cL, cX]
    , testCase "7: Sum test (completeRules)" $
        solveTest7 @?= solved
    , testCase "8: Sum test (partialRules)" $
        solveTest8 @?= Right [cBool, cInt]
    , testCase "9: Rec test (completeRules)" $
        solveTest9 @?= solved
    , testCase "10: Rec test (partialRules)" $
        solveTest10 @?= Right [cBool, cInt]
    , testCase "11: Prod test (completeRules)" $
        solveTest11 @?= solved
    , testCase "12: Prod test (partialRules)" $
        solveTest12 @?= Right [cBool, cInt]
    , testCase "13: Dec test (completeRules)" $
        solveTest13 @?= solved
    , testCase "14: Dec test (partialRules)" $
        solveTest14 @?= Right [cBool, cInt]
    , testCase "15: Overlap test (specialrules)" $
        solveTest15 @?= Left overlap
    ]
  where
    solved :: Either Overlap [R.Constraint]
    solved = Right []
    cListMaybeInt = C _c (List (Maybe Int))
    cMaybeInt = C _c (Maybe Int)
    cInt = C _c Int
    cBool = C _c Bool
    cL = C _c vl
    cX = C _c vr
    dInt = C _d Int
    overlap =
      Overlap
        cMaybeInt
        [ C _c (Maybe _X) :<= [C _c _X]
        , C _c (Maybe Int) :<= []
        ]

-- hardcoded PATTERN variables, easier to read than (VarP "blah") everywhere
_X, _XS, _VARS, _NAME, _L, _BODY :: Pat
_X = VarP "x"
_XS = VarP "xs"
_VARS = VarP "vars"
_NAME = VarP "name"
_L = VarP "l"
_BODY = VarP "body"

-- hardcoded TYPE variables
vl, vr :: Pat
vl = TyVarP "l"
vr = TyVarP "r"

-- TODO(@gnumonik): Import this from somewhere else when that somewhere else module exists
mkStructuralRules :: Class -> [Rule]
mkStructuralRules c =
  [ C c Nil :<= []
  , C c (_X :* _XS) :<= [C c _X, C c _XS]
  , C c (_L := _X) :<= [C c _X]
  , C c (RecP _XS) :<= [C c _XS]
  , C c (ProdP _XS) :<= [C c _XS]
  , C c (SumP _XS) :<= [C c _XS]
  , C c (DecP _NAME _VARS _BODY) :<= [C c _BODY]
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
  , C c (Maybe _X) :<= [C c _X]
  , C c (Either _L _X) :<= [C c _L, C c _X]
  , C c (List _X) :<= [C c _X]
  ]

userRules2 :: Class -> [Rule]
userRules2 c =
  [ C c (Maybe _X) :<= [C c _X]
  , C c (Either _L _X) :<= [C c _L, C c _X]
  , C c (List _X) :<= [C c _X]
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
solveTest1 :: Either Overlap [R.Constraint]
solveTest1 = solve (completeRules _c) (C _c $ List (Maybe Int))

-- D [Maybe Int] w/ incomplete rules (expected: [C [Maybe Int], C (Maybe Int), D Int])
solveTest2 :: Either Overlap [R.Constraint]
solveTest2 = solve (partialRules _d) (C _d $ List (Maybe Int))

-- D [Maybe Int] w/ complete D rules & incomplete C rules (expected: [C Int])
solveTest3 :: Either Overlap [R.Constraint]
solveTest3 = solve rules (C _d $ List (Maybe Int))
  where
    rules = completeRules _d <> partialRules _c

-- C [[[Bool]]] w/ complete rules (expected: [])
solveTest4 :: Either Overlap [R.Constraint]
solveTest4 = solve (completeRules _c) $ C _c $ List (List (List Bool))

-- C (Either (Either Int Bool) (Either Bool Int)) w/ complete rules (expected: [])
solveTest5 :: Either Overlap [R.Constraint]
solveTest5 = solve (completeRules _c) $ C _c $ Either (Either Int Bool) (Either Bool Int)

-- NOTE(@bladyjoker): This one shows that we never need to "freshen" variables
-- C (Either l x) w/ complete rules (expected: [C l, C x])
solveTest6 :: Either Overlap [R.Constraint]
solveTest6 = solve (completeRules _c) $ C _c $ Either vl vr

-- tests for structural subcomponents of types. Can't write Haskell equivalents (w/o row-types)

-- expected: []
solveTest7 :: Either Overlap [R.Constraint]
solveTest7 = solve (completeRules _c) $ C _c sumBody
  where
    sumBody = SumP $ Labeled "Ctor1" Int :* Labeled "Ctor2" (List Bool) :* Nil

-- expected [C Bool, C Int]
solveTest8 :: Either Overlap [R.Constraint]
solveTest8 = solve (partialRules _c) $ C _c sumBody
  where
    sumBody = SumP $ Labeled "Ctor1" Int :* Labeled "Ctor2" (List Bool) :* Nil

-- expected []
solveTest9 :: Either Overlap [R.Constraint]
solveTest9 = solve (completeRules _c) $ C _c recBody
  where
    recBody =
      RecP $
        Labeled "field1" (Maybe Bool)
          :* Labeled "field2" (Either Int (List Int))
          :* Nil

-- expected [C Bool, C Int]
solveTest10 :: Either Overlap [R.Constraint]
solveTest10 = solve (partialRules _c) $ C _c recBody
  where
    recBody =
      RecP $
        Labeled "field1" (Maybe Bool)
          :* Labeled "field2" (Either Int (List Int))
          :* Nil

-- expected []
solveTest11 :: Either Overlap [R.Constraint]
solveTest11 = solve (completeRules _c) $ C _c prodBody
  where
    prodBody = ProdP $ List Bool :* Either Int Bool :* Nil

-- expected [C Bool, C Int]
solveTest12 :: Either Overlap [R.Constraint]
solveTest12 = solve (partialRules _c) $ C _c prodBody
  where
    prodBody = ProdP $ List Bool :* Either Int Bool :* Nil

-- expected []
solveTest13 :: Either Overlap [R.Constraint]
solveTest13 = solve (completeRules _c) $ C _c testDec
  where
    testDec =
      DecP (Name "Foo") Nil $
        SumP $
          Labeled "Ctor1" (ProdP $ Maybe Int :* Nil)
            :* Labeled "Ctor2" (RecP $ Labeled "field1" Bool :* Nil)
            :* Nil

-- expected [C Int, C Bool]
solveTest14 :: Either Overlap [R.Constraint]
solveTest14 = solve (partialRules _c) $ C _c testDec
  where
    testDec =
      DecP (Name "Foo") Nil $
        SumP $
          Labeled "Ctor1" (ProdP $ Maybe Int :* Nil)
            :* Labeled "Ctor2" (RecP $ Labeled "field1" Bool :* Nil)
            :* Nil

-- expected overlap
solveTest15 :: Either Overlap [R.Constraint]
solveTest15 = solve cOverlapRules (C _c $ List (Maybe Int))
  where
    cOverlapRules =
      [ C _c (Maybe _X) :<= [C _c _X]
      , C _c (Maybe Int) :<= []
      , C _c Int :<= []
      , C _c (List _X) :<= [C _c _X]
      ]
