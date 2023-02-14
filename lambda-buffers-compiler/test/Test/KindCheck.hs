module Test.KindCheck (test) where

import Data.Bifunctor (Bifunctor (bimap))
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
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P

import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  forAll,
  forAllShrink,
  resize,
  shuffle,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Utils.CompilerInput (
  compilerInput'doubleDeclaration,
  compilerInput'doubleDeclarationDiffMod,
  compilerInput'incoherent,
  compilerInput'maybe,
 )
import Test.Utils.Constructors (_ModuleName)
import Test.Utils.TyDef (tyDef'maybe)

--------------------------------------------------------------------------------
-- Top Level tests

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds, testRefl, testMultipleDec]

--------------------------------------------------------------------------------
-- Multiple declaration test

testMultipleDec :: TestTree
testMultipleDec = testGroup "Multiple declaration tests." [doubleDeclaration, passingDoubleDeclaration]

doubleDeclaration :: TestTree
doubleDeclaration =
  testCase "Two declarations of Maybe within the same module are caught." $
    check_ compilerInput'doubleDeclaration
      @?= Left (P.CompKindCheckError (P.MultipleTyDefError moduleName [tyDef'maybe, tyDef'maybe]))
  where
    moduleName = _ModuleName ["Module"]

passingDoubleDeclaration :: TestTree
passingDoubleDeclaration =
  testCase "Two declarations of Maybe within different modules are fine." $
    check_ compilerInput'doubleDeclarationDiffMod @?= Right ()

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
  testProperty "Module order inside the CompilerInput does not matter." $
    forAllShrink (resize 5 genModuleIn2Layouts) shrink $
      \(l, r) -> eitherFailOrPass (check_ l) == eitherFailOrPass (check_ r)
  where
    genModuleIn2Layouts = do
      mods <- arbitrary
      shuffledMods <- shuffle mods
      pure (P.CompilerInput mods, P.CompilerInput shuffledMods)

eitherFailOrPass :: forall {a} {c}. Either a c -> Either () ()
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

testPProdFoldTotal :: TestTree
testPProdFoldTotal =
  testProperty "ProductFold is total." $
    forAll arbitrary $
      \ts -> foldWithProduct ts === foldWithProduct ts

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

-- Property Tests
testRefl :: TestTree
testRefl = testProperty "Refl" reflTerm
  where
    reflTerm :: Property
    reflTerm = forAllShrink (arbitrary @Int) shrink (\a -> a == a)
