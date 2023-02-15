{-# LANGUAGE PatternSynonyms #-}

module Test.TypeClassCheck where

import Control.Lens ((.~))
import Data.Function ((&))
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Compiler.ProtoCompat (IsMessage (fromProto))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as ProtoCompat
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )
import LambdaBuffers.Compiler.TypeClass.Rules qualified as R
import LambdaBuffers.Compiler.TypeClass.Solve (Overlap (Overlap), solve)
import LambdaBuffers.Compiler.TypeClass.Validate (
  mkStructuralRules,
  _L,
  _X,
 )
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
    solved :: Either Overlap [R.Constraint Exp]
    solved = Right []
    cListMaybeInt = C _c (ListE (MaybeE IntE))
    cMaybeInt = C _c (MaybeE IntE)
    cInt = C _c IntE
    cBool = C _c BoolE
    cL = C _c vl
    cX = C _c vr
    dInt = C _d IntE
    overlap =
      Overlap
        cMaybeInt
        [ C _c (MaybeP _X) :<= [C _c _X]
        , C _c (MaybeP IntP) :<= []
        ]

-- hardcoded TYPE variables
vl, vr :: Exp
vl = LitE (TyVar "l")
vr = LitE (TyVar "r")

-- Template
pattern (:@) :: Pat -> Pat -> Pat
pattern (:@) p1 p2 = AppP p1 p2

pattern LocalRefP :: Text -> Pat
pattern LocalRefP nm = RefP NilP (LitP (Name nm))

pattern MaybeP :: Pat -> Pat
pattern MaybeP p1 = LocalRefP "Maybe" :@ p1

pattern ListP :: Pat -> Pat
pattern ListP p = LocalRefP "List" :@ p

pattern EitherP :: Pat -> Pat -> Pat
pattern EitherP l r = (LocalRefP "Either" :@ l) :@ r

pattern IntP :: Pat
pattern IntP = LocalRefP "Int"

pattern BoolP :: Pat
pattern BoolP = LocalRefP "Bool"

pattern NoConstraints :: Class -> Pat -> Rule Pat
pattern NoConstraints c p = C c p :<= []

pattern Labeled :: Text -> Pat -> Pat
pattern Labeled nm p = LabelP (LitP (Name nm)) p

-- Target

pattern (:@@) :: Exp -> Exp -> Exp
pattern (:@@) p1 p2 = AppE p1 p2

pattern LocalRefE :: Text -> Exp
pattern LocalRefE nm = RefE NilE (LitE (Name nm))

pattern MaybeE :: Exp -> Exp
pattern MaybeE p1 = LocalRefE "Maybe" :@@ p1

pattern ListE :: Exp -> Exp
pattern ListE p = LocalRefE "List" :@@ p

pattern EitherE :: Exp -> Exp -> Exp
pattern EitherE l r = (LocalRefE "Either" :@@ l) :@@ r

pattern IntE :: Exp
pattern IntE = LocalRefE "Int"

pattern BoolE :: Exp
pattern BoolE = LocalRefE "Bool"

pattern NoConstraintsE :: Class -> Exp -> Rule Exp
pattern NoConstraintsE c p = C c p :<= []

pattern LabeledE :: Text -> Exp -> Exp
pattern LabeledE nm p = LabelE (LitE (Name nm)) p

userRules1 :: Class -> [Rule Pat]
userRules1 c =
  [ NoConstraints c IntP
  , NoConstraints c BoolP
  , C c (MaybeP _X) :<= [C c _X]
  , C c (EitherP _L _X) :<= [C c _L, C c _X]
  , C c (ListP _X) :<= [C c _X]
  ]

userRules2 :: Class -> [Rule Pat]
userRules2 c =
  [ C c (MaybeP _X) :<= [C c _X]
  , C c (EitherP _L _X) :<= [C c _L, C c _X]
  , C c (ListP _X) :<= [C c _X]
  ]

completeRules :: Class -> [Rule Pat]
completeRules c = mkStructuralRules c <> userRules1 c

partialRules :: Class -> [Rule Pat]
partialRules c = mkStructuralRules c <> userRules2 c

-- No supers
_c :: Class
_c = Class (FQClassName "C" []) []

-- C is super
_d :: Class
_d = Class (FQClassName "D" []) [_c]

-- C [Maybe Int] w/ complete rules (expected: [])
solveTest1 :: Either Overlap [R.Constraint Exp]
solveTest1 = solve (completeRules _c) (C _c $ ListE (MaybeE IntE))

-- D [Maybe Int] w/ incomplete rules (expected: [C [Maybe Int], C (Maybe Int), D Int])
solveTest2 :: Either Overlap [R.Constraint Exp]
solveTest2 = solve (partialRules _d) (C _d $ ListE (MaybeE IntE))

-- D [Maybe Int] w/ complete D rules & incomplete C rules (expected: [C Int])
solveTest3 :: Either Overlap [R.Constraint Exp]
solveTest3 = solve rules (C _d $ ListE (MaybeE IntE))
  where
    rules = completeRules _d <> partialRules _c

-- C [[[Bool]]] w/ complete rules (expected: [])
solveTest4 :: Either Overlap [R.Constraint Exp]
solveTest4 = solve (completeRules _c) $ C _c $ ListE (ListE (ListE BoolE))

-- C (Either (Either Int Bool) (Either Bool Int)) w/ complete rules (expected: [])
solveTest5 :: Either Overlap [R.Constraint Exp]
solveTest5 = solve (completeRules _c) $ C _c $ EitherE (EitherE IntE BoolE) (EitherE BoolE IntE)

-- NOTE(@bladyjoker): This one shows that we never need to "freshen" variables
-- C (Either l x) w/ complete rules (expected: [C l, C x])
solveTest6 :: Either Overlap [R.Constraint Exp]
solveTest6 = solve (completeRules _c) $ C _c $ EitherE vl vr

-- tests for structural subcomponents of types. Can't write Haskell equivalents (w/o row-types)

-- expected: []
solveTest7 :: Either Overlap [R.Constraint Exp]
solveTest7 = solve (completeRules _c) $ C _c sumBody
  where
    sumBody =
      toSumE
        [ LabeledE "Ctor1" IntE
        , LabeledE "Ctor2" (ListE BoolE)
        ]

-- expected [C Bool, C Int]
solveTest8 :: Either Overlap [R.Constraint Exp]
solveTest8 = solve (partialRules _c) $ C _c sumBody
  where
    sumBody =
      toSumE
        [ LabeledE "Ctor1" IntE
        , LabeledE "Ctor2" (ListE BoolE)
        ]

-- expected []
solveTest9 :: Either Overlap [R.Constraint Exp]
solveTest9 = solve (completeRules _c) $ C _c recBody
  where
    recBody =
      toRecE
        [ LabeledE "field1" (MaybeE BoolE)
        , LabeledE "field2" (EitherE IntE (ListE IntE))
        ]

-- expected [C Bool, C Int]
solveTest10 :: Either Overlap [R.Constraint Exp]
solveTest10 = solve (partialRules _c) $ C _c recBody
  where
    recBody =
      toRecE
        [ LabeledE "field1" (MaybeE BoolE)
        , LabeledE "field2" (EitherE IntE (ListE IntE))
        ]

-- expected []
solveTest11 :: Either Overlap [R.Constraint Exp]
solveTest11 = solve (completeRules _c) $ C _c prodBody
  where
    prodBody = toProdE [ListE BoolE, EitherE IntE BoolE]

-- expected [C Bool, C Int]
solveTest12 :: Either Overlap [R.Constraint Exp]
solveTest12 = solve (partialRules _c) $ C _c prodBody
  where
    prodBody = toProdE [ListE BoolE, EitherE IntE BoolE]

-- expected []
solveTest13 :: Either Overlap [R.Constraint Exp]
solveTest13 = solve (completeRules _c) $ C _c testDec
  where
    testDec =
      DecE (LitE (Name "Foo")) NilE $
        toSumE
          [ LabeledE "Ctor1" (toProdE [MaybeE IntE])
          , LabeledE "Ctor2" (toRecE [LabeledE "field1" BoolE])
          ]

-- expected [C Int, C Bool]
solveTest14 :: Either Overlap [R.Constraint Exp]
solveTest14 = solve (partialRules _c) $ C _c testDec
  where
    testDec =
      DecE (LitE (Name "Foo")) NilE $
        toSumE
          [ LabeledE "Ctor1" (toProdE [MaybeE IntE])
          , LabeledE "Ctor2" (toRecE [LabeledE "field1" BoolE])
          ]

-- expected overlap
solveTest15 :: Either Overlap [R.Constraint Exp]
solveTest15 = solve cOverlapRules (C _c $ ListE (MaybeE IntE))
  where
    cOverlapRules =
      [ C _c (MaybeP _X) :<= [C _c _X]
      , C _c (MaybeP IntP) :<= []
      , C _c IntP :<= []
      , C _c (ListP _X) :<= [C _c _X]
      ]

{-
    checkDerive tests
-}

{- Test 1:

module A where

opaque Int
opaque Bool
opaque

-}
