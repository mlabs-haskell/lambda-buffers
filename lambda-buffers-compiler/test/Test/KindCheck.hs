module Test.KindCheck (test) where

import LambdaBuffers.Compiler.KindCheck (
  check_,
  foldWithArrowToType,
 )

import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, (:->:)))
import Test.KindCheck.Errors (testGKindCheckErrors)
import Test.QuickCheck (Arbitrary (arbitrary), forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Utils.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
 )
import Test.Utils.Constructors (_CompilerInput)

--------------------------------------------------------------------------------
-- Top Level tests

test :: TestTree
test =
  testGroup
    "Compiler tests"
    [ testCheck
    , testFolds
    , testGKindCheckErrors
    ]

--------------------------------------------------------------------------------
-- Module tests

testCheck :: TestTree
testCheck =
  testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing]

trivialKCTest :: TestTree
trivialKCTest =
  testCase "Empty CompInput should check" $
    check_ (_CompilerInput []) @?= Right ()

kcTestMaybe :: TestTree
kcTestMaybe =
  testCase "Maybe should pass" $
    check_ compilerInput'maybe @?= Right ()

kcTestFailing :: TestTree
kcTestFailing =
  testCase "This should fail" $
    assertBool "Test should have failed" $
      check_ compilerInput'incoherent /= Right ()

--------------------------------------------------------------------------------
-- Fold tests

testFolds :: TestTree
testFolds =
  testGroup
    "Test Folds"
    [ testGroup
        "Test Arrow Folds"
        [testArrowFold0, testArrowFold1, testArrowFold2, testArrowFold3HK, testArrowFold4HK, testArrowFoldHHK, testFoldWithArrowToTypeTotal]
    ]

ty :: Kind
ty = KType

-- | [ ] -> *
testArrowFold0 :: TestTree
testArrowFold0 =
  testCase "Fold 0 kinds" $
    foldWithArrowToType [] @?= ty

-- | [*] => * -> *
testArrowFold1 :: TestTree
testArrowFold1 =
  testCase "Fold 1 kinds" $
    foldWithArrowToType [ty] @?= ty :->: ty

-- | [*,*] => * -> * -> *
testArrowFold2 :: TestTree
testArrowFold2 =
  testCase "Fold 2 kinds" $
    foldWithArrowToType [ty, ty] @?= ty :->: (ty :->: ty)

-- | [* -> *, * ] => (* -> *) -> * -> *
testArrowFold3HK :: TestTree
testArrowFold3HK =
  testCase "Fold 2 HKT" $
    foldWithArrowToType [ty :->: ty, ty]
      @?= ((ty :->: ty) :->: (ty :->: ty))

-- | [*, * -> *, * ] => * -> (* -> *) -> * -> *
testArrowFold4HK :: TestTree
testArrowFold4HK =
  testCase "Fold 2 HKT" $
    foldWithArrowToType [ty, ty :->: ty, ty]
      @?= (ty :->: ((ty :->: ty) :->: (ty :->: ty)))

-- | [*, * -> *, * ] => * -> ((* -> *) -> *) -> * -> *
testArrowFoldHHK :: TestTree
testArrowFoldHHK =
  testCase "Fold 2 HKT" $
    foldWithArrowToType [ty, (ty :->: ty) :->: ty, ty]
      @?= (ty :->: (((ty :->: ty) :->: ty) :->: (ty :->: ty)))

testFoldWithArrowToTypeTotal :: TestTree
testFoldWithArrowToTypeTotal =
  testProperty "foldWithArrowToType is total" $
    forAll arbitrary $
      \ts -> foldWithArrowToType ts === foldWithArrowToType ts
