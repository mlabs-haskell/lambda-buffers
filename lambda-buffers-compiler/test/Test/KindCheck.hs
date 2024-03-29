module Test.KindCheck (test) where

import LambdaBuffers.Compiler.KindCheck (
  foldWithArrowToType,
  runCheck,
 )

import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen (choice, int, list)
import Hedgehog.Range qualified as R
import LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, KVar, (:->:)))
import Test.KindCheck.Errors (testGKindCheckErrors)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Test.Utils.CompilerInput (
  compilerInput'either,
  compilerInput'incoherent,
  compilerInput'maybe,
  compilerInput'newTypeEither,
  compilerInput'newTypeEither',
  compilerInput'newTypeEither'',
  compilerInput'recDef,
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
  testGroup
    "KindChecker Tests"
    [ trivialKCTest
    , kcTestMaybe
    , kcTestFailing
    , kcTestEither
    , kcTestMaybe'n'Either
    , kcTestRec
    , kcWrappedTestEither
    , kcWrappedTestEither'
    , kcWrappedTestEither''
    ]

trivialKCTest :: TestTree
trivialKCTest =
  testCase "Empty Compiler Input kind checks." $
    runCheck (_CompilerInput []) @?= Right ()

kcTestMaybe :: TestTree
kcTestMaybe =
  testCase "Kind check Maybe." $
    runCheck compilerInput'maybe @?= Right ()

kcTestEither :: TestTree
kcTestEither =
  testCase "Kind check Either." $
    runCheck compilerInput'either @?= Right ()

kcWrappedTestEither :: TestTree
kcWrappedTestEither =
  testCase "Kind check Either + Wrapped Either." $
    runCheck compilerInput'newTypeEither @?= Right ()

kcWrappedTestEither' :: TestTree
kcWrappedTestEither' =
  testCase "Kind check Either (defined Opaque) + Wrapped Either." $
    runCheck compilerInput'newTypeEither' @?= Right ()

kcWrappedTestEither'' :: TestTree
kcWrappedTestEither'' =
  testCase "Kind check Either + Wrapped (Either Int Int) ." $
    runCheck compilerInput'newTypeEither'' @?= Right ()

kcTestMaybe'n'Either :: TestTree
kcTestMaybe'n'Either =
  testCase "Kind check Maybe and Either." $
    runCheck (compilerInput'maybe <> compilerInput'either) @?= Right ()

kcTestRec :: TestTree
kcTestRec =
  testCase "Kind check recursive def." $
    runCheck compilerInput'recDef @?= Right ()

kcTestFailing :: TestTree
kcTestFailing =
  testCase "This should fail" $
    assertBool "Test should have failed" $
      runCheck compilerInput'incoherent /= Right ()

--------------------------------------------------------------------------------
-- Fold tests

testFolds :: TestTree
testFolds =
  testGroup
    "Test Folds"
    [ testGroup
        "Test Arrow Folds"
        [ testArrowFold0
        , testArrowFold1
        , testArrowFold2
        , testArrowFold3HK
        , testArrowFold4HK
        , testArrowFoldHHK
        , testFoldWithArrowToTypeTotal
        ]
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

testFoldWithArrowToTypeTotal :: HasCallStack => TestTree
testFoldWithArrowToTypeTotal =
  testProperty
    "foldWithArrowToType is total"
    ( property $ do
        ts <- forAll genKinds
        foldWithArrowToType ts === foldWithArrowToType ts
    )
  where
    genKind :: Gen Kind
    genKind =
      choice
        [ return KType
        , KVar . toInteger <$> int (R.constant 0 100)
        , (:->:) <$> genKind <*> genKind
        ]

    genKinds :: Gen [Kind]
    genKinds = list (R.constant 0 10) genKind
