{-# LANGUAGE PatternSynonyms #-}

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
import LambdaBuffers.Compiler.TypeClassCheck.Pat (
  Exp (AppE, LabelE, LitE, NilE, RefE),
  Literal (Name, TyVar),
  Pat (AppP, LitP, NilP, RefP),
  Tagged,
  toProdE,
  toRecE,
  toSumE,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Rules qualified as R
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap (Overlap), defTag, solve)
import LambdaBuffers.Compiler.TypeClassCheck.Validate (
  mkStructuralRules,
  _L,
  _X,
 )
import Proto.Compiler (ClassDef, CompilerInput, Constraint, Kind, Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler_Fields (argKind, argName, args, classArgs, classDefs, className, classRef, kindRef, localClassRef, moduleName, modules, name, parts, supers, tyVar, varName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Utils.Constructors (_ModuleName)

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

constraint :: Text -> Proto.Compiler.Constraint
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

---- Solver tests

solveTests :: TestTree
solveTests =
  testGroup
    "Solver tests"
    [ testCase "C [Maybe Int] (completeRules)" $
        solveTest1 @?= solved
    , testCase "D [Maybe Int] (partialRules)" $
        solveTest2 @?= Right (defTag <$> [cListMaybeInt, cMaybeInt, dInt])
    , testCase "D [Maybe Int] (complete D, partial C)" $
        solveTest3 @?= Right (defTag <$> [cInt])
    , testCase "C [[[Bool]]] (completeRules)" $
        solveTest4 @?= solved
    , testCase "C (Either (Either Int Bool) (Either Bool Int)) (completeRules)" $
        solveTest5 @?= solved
    , testCase "C (Either l x) (completeRules)" $
        solveTest6 @?= solved
    , testCase "Sum test (completeRules)" $
        solveTest7 @?= solved
    , testCase "Sum test (partialRules)" $
        solveTest8 @?= Right (defTag <$> [cBool, cInt])
    , testCase "Rec test (completeRules)" $
        solveTest9 @?= solved
    , testCase "Rec test (partialRules)" $
        solveTest10 @?= Right (defTag <$> [cBool, cInt])
    , testCase "Prod test (completeRules)" $
        solveTest11 @?= solved
    , testCase "Prod test (partialRules)" $
        solveTest12 @?= Right (defTag <$> [cBool, cInt])
    , testCase "Overlap test (specialRules)" $
        solveTest13
          @?= Left
            ( Overlap
                (defTag cMaybeInt)
                ( defTag
                    <$> [ C _c (MaybeP _X) :<= [C _c _X]
                        , C _c (MaybeP IntP) :<= []
                        ]
                )
            )
    ]
  where
    solved :: Either Overlap [Tagged (R.Constraint Exp)]
    solved = Right []
    cListMaybeInt = C _c (ListE (MaybeE IntE))
    cMaybeInt = C _c (MaybeE IntE)
    cInt = C _c IntE
    cBool = C _c BoolE
    dInt = C _d IntE

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

pattern LabeledE :: Text -> Exp -> Exp
pattern LabeledE nm p = LabelE (LitE (Name nm)) p

userRules1 :: Class -> [Tagged (Rule Pat)]
userRules1 c =
  defTag
    <$> [ NoConstraints c IntP
        , NoConstraints c BoolP
        , C c (MaybeP _X) :<= [C c _X]
        , C c (EitherP _L _X) :<= [C c _L, C c _X]
        , C c (ListP _X) :<= [C c _X]
        ]

userRules2 :: Class -> [Tagged (Rule Pat)]
userRules2 c =
  defTag
    <$> [ C c (MaybeP _X) :<= [C c _X]
        , C c (EitherP _L _X) :<= [C c _L, C c _X]
        , C c (ListP _X) :<= [C c _X]
        ]

completeRules :: Class -> [Tagged (Rule Pat)]
completeRules c = mkStructuralRules c <> userRules1 c

partialRules :: Class -> [Tagged (Rule Pat)]
partialRules c = mkStructuralRules c <> userRules2 c

-- No supers
_c :: Class
_c = Class (FQClassName "C" []) []

-- C is super
_d :: Class
_d = Class (FQClassName "D" []) [_c]

-- C [Maybe Int] w/ complete rules (expected: [])
solveTest1 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest1 = solve (completeRules _c) (defTag $ C _c $ ListE (MaybeE IntE))

-- D [Maybe Int] w/ incomplete rules (expected: [C [Maybe Int], C (Maybe Int), D Int])
solveTest2 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest2 = solve (partialRules _d) (defTag $ C _d $ ListE (MaybeE IntE))

solveTest3 :: Either Overlap [Tagged (R.Constraint Exp)]
-- D [Maybe Int] w/ complete D rules & incomplete C rules (expected: [C Int])
solveTest3 = solve rules (defTag $ C _d $ ListE (MaybeE IntE))
  where
    rules = completeRules _d <> partialRules _c
solveTest4 :: Either Overlap [Tagged (R.Constraint Exp)]
-- C [[[Bool]]] w/ complete rules (expected: [])
solveTest5 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest4 = solve (completeRules _c) $ defTag $ C _c $ ListE (ListE (ListE BoolE))

-- C (Either (Either Int Bool) (Either Bool Int)) w/ complete rules (expected: [])
solveTest5 = solve (completeRules _c) $ defTag $ C _c $ EitherE (EitherE IntE BoolE) (EitherE BoolE IntE)

-- NOTE: This passes as a result of our hack where we assume that instances
--       for bare type variables are satisfied.
-- C (Either l x) w/ complete rules (expected: [])
solveTest6 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest6 = solve (completeRules _c) $ defTag $ C _c $ EitherE vl vr

-- tests for structural subcomponents of types. Can't write Haskell equivalents (w/o row-types)

-- expected: []
solveTest7 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest7 = solve (completeRules _c) $ defTag $ C _c sumBody
  where
    sumBody =
      toSumE
        [ LabeledE "Ctor1" IntE
        , LabeledE "Ctor2" (ListE BoolE)
        ]

solveTest8 :: Either Overlap [Tagged (R.Constraint Exp)]
-- expected [C Bool, C Int]
solveTest8 = solve (partialRules _c) $ defTag $ C _c sumBody
  where
    sumBody =
      toSumE
        [ LabeledE "Ctor1" IntE
        , LabeledE "Ctor2" (ListE BoolE)
        ]

-- expected []
solveTest9 :: Either Overlap [Tagged (R.Constraint Exp)]
solveTest9 = solve (completeRules _c) $ defTag $ C _c recBody
  where
    recBody =
      toRecE
        [ LabeledE "field1" (MaybeE BoolE)
        , LabeledE "field2" (EitherE IntE (ListE IntE))
        ]

solveTest10 :: Either Overlap [Tagged (R.Constraint Exp)]
-- expected [C Bool, C Int]
solveTest10 = solve (partialRules _c) $ defTag $ C _c recBody
  where
    recBody =
      toRecE
        [ LabeledE "field1" (MaybeE BoolE)
        , LabeledE "field2" (EitherE IntE (ListE IntE))
        ]

solveTest11 :: Either Overlap [Tagged (R.Constraint Exp)]
-- expected []
solveTest11 = solve (completeRules _c) $ defTag $ C _c prodBody
  where
    prodBody = toProdE [ListE BoolE, EitherE IntE BoolE]
solveTest12 :: Either Overlap [Tagged (R.Constraint Exp)]
-- expected [C Bool, C Int]
solveTest12 = solve (partialRules _c) $ defTag $ C _c prodBody
  where
    prodBody = toProdE [ListE BoolE, EitherE IntE BoolE]

solveTest13 :: Either Overlap [Tagged (R.Constraint Exp)]
-- expected overlap
solveTest13 = solve cOverlapRules (defTag $ C _c $ ListE (MaybeE IntE))
  where
    cOverlapRules =
      defTag
        <$> [ C _c (MaybeP _X) :<= [C _c _X]
            , C _c (MaybeP IntP) :<= []
            , C _c IntP :<= []
            , C _c (ListP _X) :<= [C _c _X]
            ]
