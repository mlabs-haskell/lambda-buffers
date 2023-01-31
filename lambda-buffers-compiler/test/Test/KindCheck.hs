{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.KindCheck (test) where

import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import LambdaBuffers.Compiler.KindCheck (
  check_,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples (compilerInput'incoherent, compilerInput'maybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds]

--------------------------------------------------------------------------------
-- Module tests

testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing]

trivialKCTest =
  testCase "Empty CompInput should check." $
    check_ (P.CompilerInput []) @?= Right ()

kcTestMaybe =
  testCase "Maybe should pass." $
    check_ compilerInput'maybe @?= Right ()

kcTestFailing =
  testCase "This should fail." $
    assertBool "Test should have failed." $
      check_ compilerInput'incoherent /= Right ()

--------------------------------------------------------------------------------
-- Fold tests

testFolds =
  testGroup
    "Test Folds"
    [ testGroup "Test Product Folds." [testFoldProd1, testFoldProd2, testFoldProd3]
    , testGroup "Test Sum Folds." [testSumFold1, testSumFold2, testSumFold3]
    ]

-- | [ a ] -> a
testFoldProd1 =
  testCase "Fold with product - 1 type." $
    foldWithProduct (Var "a" :| []) @?= Var "a"

-- | [a ,b] -> (a,b)
testFoldProd2 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (Var "b") $ Var "a" :| [])
      @?= App (App (Var "Π") (Var "b")) (Var "a")

-- | [ a, b ,c ] -> (a,(b,c))
testFoldProd3 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (Var "c") $ cons (Var "b") $ Var "a" :| [])
      @?= App
        (App (Var "Π") (Var "c"))
        (App (App (Var "Π") (Var "b")) (Var "a"))

-- | [ a ] -> a
testSumFold1 =
  testCase "Fold 1 type." $
    foldWithSum (Var "a" :| []) @?= Var "a"

-- | [ a , b ] -> a | b
testSumFold2 =
  testCase "Fold 2 type." $
    foldWithSum (cons (Var "b") $ Var "a" :| [])
      @?= App (App (Var "Σ") (Var "b")) (Var "a")

-- | [ a , b , c ] -> a | ( b | c )
testSumFold3 =
  testCase "Fold 3 types." $
    foldWithSum (cons (Var "c") $ cons (Var "b") $ Var "a" :| [])
      @?= App
        (App (Var "Σ") (Var "c"))
        (App (App (Var "Σ") (Var "b")) (Var "a"))
