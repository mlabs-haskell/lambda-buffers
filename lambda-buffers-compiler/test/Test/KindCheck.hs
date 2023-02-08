module Test.KindCheck (test) where

import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import Data.Text (Text)
import LambdaBuffers.Compiler.KindCheck (
  check_,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var))
import LambdaBuffers.Compiler.KindCheck.Variable (
  Variable (LocalRef),
 )
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  CompilerInput (CompilerInput),
 )
import Test.Samples.Proto.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds]

--------------------------------------------------------------------------------
-- Module tests

testCheck :: TestTree
testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing]

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

--------------------------------------------------------------------------------
-- Fold tests

testFolds :: TestTree
testFolds =
  testGroup
    "Test Folds"
    [ testGroup "Test Product Folds." [testFoldProd1, testFoldProd2, testFoldProd3]
    , testGroup "Test Sum Folds." [testSumFold1, testSumFold2, testSumFold3]
    ]

-- | [ a ] -> a
testFoldProd1 :: TestTree
testFoldProd1 =
  testCase "Fold with product - 1 type." $
    foldWithProduct (lVar "a" :| []) @?= lVar "a"

-- | [a ,b] -> (a,b)
testFoldProd2 :: TestTree
testFoldProd2 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (lVar "b") $ lVar "a" :| [])
      @?= App (App (lVar "Π") (lVar "b")) (lVar "a")

-- | [ a, b ,c ] -> (a,(b,c))
testFoldProd3 :: TestTree
testFoldProd3 =
  testCase "Fold with product - 2 types." $
    foldWithProduct (cons (lVar "c") $ cons (lVar "b") $ lVar "a" :| [])
      @?= App
        (App (lVar "Π") (lVar "c"))
        (App (App (lVar "Π") (lVar "b")) (lVar "a"))

-- | [ a ] -> a
testSumFold1 :: TestTree
testSumFold1 =
  testCase "Fold 1 type." $
    foldWithSum (lVar "a" :| []) @?= lVar "a"

-- | [ a , b ] -> a | b
testSumFold2 :: TestTree
testSumFold2 =
  testCase "Fold 2 type." $
    foldWithSum (cons (lVar "b") $ lVar "a" :| [])
      @?= App (App (lVar "Σ") (lVar "b")) (lVar "a")

-- | [ a , b , c ] -> a | ( b | c )
testSumFold3 :: TestTree
testSumFold3 =
  testCase "Fold 3 types." $
    foldWithSum (cons (lVar "c") $ cons (lVar "b") $ lVar "a" :| [])
      @?= App
        (App (lVar "Σ") (lVar "c"))
        (App (App (lVar "Σ") (lVar "b")) (lVar "a"))

-- | TyDef to Kind Canonical representation - sums not folded - therefore we get constructor granularity. Might use in a different implementation for more granular errors.
lVar :: Text -> Type
lVar = Var . LocalRef
