module Test.KindCheck (test) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import LambdaBuffers.Compiler.KindCheck (
  check_,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  CompilerInput (CompilerInput),
 )
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  forAll,
  forAllShrink,
  resize,
  shuffle,
  (===),
 )
import Test.Samples.Proto.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------
-- Top Level tests

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds, testRefl]

--------------------------------------------------------------------------------
-- Module tests

testCheck :: TestTree
testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing, kcTestOrdering]

trivialKCTest :: TestTree
trivialKCTest =
  testCase "Empty CompInput should check." $
    check_ (P.CompilerInput []) @?= Right ()

kcTestMaybe :: TestTree
kcTestMaybe =
  testCase "Maybe should pass." $
    check_ compilerInput'maybe @?= Right ()

kcTestFailing :: TestTree
kcTestFailing =
  testCase "This should fail." $
    assertBool "Test should have failed." $
      check_ compilerInput'incoherent /= Right ()

{- | TyDef order does not matter when kind checking.

 We're not interested in the failure error as there might be more than two
 errors in a module - and it is non-determistic which one is first. But it is
 deterministic if the property holds for the whole CompilerInput. Therefore we
 only track if given the input - the fails or succeeds.
-}
kcTestOrdering :: TestTree
kcTestOrdering =
  testProperty "Module order inside the CompilerInput does not matter to the result of the kindchecker." $
    forAllShrink (resize 5 genModuleIn2Layouts) shrink $
      \(l, r) -> eitherFailOrPass (check_ l) == eitherFailOrPass (check_ r)
  where
    genModuleIn2Layouts = do
      mods <- arbitrary
      shuffledMods <- shuffle mods
      pure (P.CompilerInput mods, P.CompilerInput shuffledMods)

    eitherFailOrPass = bimap (const ()) (const ())

--------------------------------------------------------------------------------
-- Fold tests

testFolds :: TestTree
testFolds =
  testGroup
    "Test Folds"
    [ testGroup "Test Product Folds." [testFoldProd1, testFoldProd2, testFoldProd3, testPProdFoldTotal]
    , testGroup "Test Sum Folds." [testSumFold1, testSumFold2, testSumFold3]
    ]

-- | [ a ] -> a
testFoldProd1 :: TestTree
testFoldProd1 =
  testCase "Fold with product - 1 type." $
    foldWithProduct (Var "a" :| []) @?= Var "a"

-- | [a ,b] -> (a,b)
testFoldProd2 :: TestTree
testFoldProd2 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (Var "b") $ Var "a" :| [])
      @?= App (App (Var "Π") (Var "b")) (Var "a")

-- | [ a, b ,c ] -> (a,(b,c))
testFoldProd3 :: TestTree
testFoldProd3 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (Var "c") $ cons (Var "b") $ Var "a" :| [])
      @?= App
        (App (Var "Π") (Var "c"))
        (App (App (Var "Π") (Var "b")) (Var "a"))

testPProdFoldTotal :: TestTree
testPProdFoldTotal =
  testProperty "ProductFold is total." $
    forAll arbitrary $
      \ts -> foldWithProduct ts === foldWithProduct ts

-- | [ a ] -> a
testSumFold1 :: TestTree
testSumFold1 =
  testCase "Fold 1 type." $
    foldWithSum (Var "a" :| []) @?= Var "a"

-- | [ a , b ] -> a | b
testSumFold2 :: TestTree
testSumFold2 =
  testCase "Fold 2 type." $
    foldWithSum (cons (Var "b") $ Var "a" :| [])
      @?= App (App (Var "Σ") (Var "b")) (Var "a")

-- | [ a , b , c ] -> a | ( b | c )
testSumFold3 :: TestTree
testSumFold3 =
  testCase "Fold 3 types." $
    foldWithSum (cons (Var "c") $ cons (Var "b") $ Var "a" :| [])
      @?= App
        (App (Var "Σ") (Var "c"))
        (App (App (Var "Σ") (Var "b")) (Var "a"))

-- Property Tests
testRefl :: TestTree
testRefl = testProperty "Refl" reflTerm
  where
    reflTerm :: Property
    reflTerm = forAllShrink (arbitrary @Int) shrink (\a -> a == a)
