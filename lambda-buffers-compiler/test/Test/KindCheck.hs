module Test.KindCheck (test) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (Text)
import LambdaBuffers.Compiler.KindCheck (
  check_,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var), tyEither, tyProd, tyUnit, tyVoid)
import LambdaBuffers.Compiler.KindCheck.Variable (
  Variable (LocalRef),
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
test = testGroup "Compiler tests" [testCheck, testFolds, testRefl]

--------------------------------------------------------------------------------
-- Module tests

testCheck :: TestTree
testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing, kcTestOrdering]

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

{- | TyDef order does not matter when kind checking.

 We're not interested in the failure error as there might be more than two
 errors in a module - and it is non-determistic which one is first. But it is
 deterministic if the property holds for the whole CompilerInput. Therefore we
 only track if given the input - the fails or succeeds.
-}
kcTestOrdering :: TestTree
kcTestOrdering =
  testProperty "Module order inside the CompilerInput does not matter" $
    forAllShrink (resize 2 genModuleIn2Layouts) shrink $
      \(l, r) -> check_ l === check_ r
  where
    genModuleIn2Layouts = do
      mods <- arbitrary
      shuffledMods <- shuffle mods
      pure (_CompilerInput mods, _CompilerInput shuffledMods)

eitherFailOrPass :: forall {a} {c}. Either a c -> Either () ()
eitherFailOrPass = bimap (const ()) (const ())

--------------------------------------------------------------------------------
-- Fold tests

testFolds :: TestTree
testFolds =
  testGroup
    "Test Folds"
    [ testGroup "Test Product Folds" [testFoldProd0, testFoldProd1, testFoldProd2, testFoldProd3, testPProdFoldTotal]
    , testGroup "Test Sum Folds" [testSumFold0, testSumFold1, testSumFold2, testSumFold3]
    ]

prod :: Type -> Type -> Type
prod = App . App (Var tyProd)

unit' :: Type
unit' = Var tyUnit

-- | [ ] -> unit
testFoldProd0 :: TestTree
testFoldProd0 =
  testCase "Fold with product - 0 type" $
    foldWithProduct [] @?= unit'

-- | [ a ] -> prod unit a
testFoldProd1 :: TestTree
testFoldProd1 =
  testCase "Fold with product - 1 type" $
    foldWithProduct [lVar "a"] @?= prod unit' (lVar "a")

-- | [b ,a] -> prod (prod unit b) a
testFoldProd2 :: TestTree
testFoldProd2 =
  testCase "Fold with product - 2 types" $
    foldWithProduct [lVar "b", lVar "a"]
      @?= prod (prod unit' (lVar "b")) (lVar "a")

-- | [ a, b ,c ] -> prod (prod (prod unit c) b) a
testFoldProd3 :: TestTree
testFoldProd3 =
  testCase "Fold with product - 2 types" $
    foldWithProduct [lVar "c", lVar "b", lVar "a"]
      @?= prod (prod (prod unit' (lVar "c")) (lVar "b")) (lVar "a")

testPProdFoldTotal :: TestTree
testPProdFoldTotal =
  testProperty "ProductFold is total" $
    forAll arbitrary $
      \ts -> foldWithProduct ts === foldWithProduct ts

either' :: Type -> Type -> Type
either' = App . App (Var tyEither)

void' :: Type
void' = Var tyVoid

-- | [ ] -> void
testSumFold0 :: TestTree
testSumFold0 =
  testCase "Fold 0 type" $
    foldWithSum [] @?= void'

-- | [ a ] -> either void a
testSumFold1 :: TestTree
testSumFold1 =
  testCase "Fold 1 type" $
    foldWithSum [lVar "a"] @?= either' void' (lVar "a")

-- | [ a , b ] -> either (either void a) b
testSumFold2 :: TestTree
testSumFold2 =
  testCase "Fold 2 type" $
    foldWithSum [lVar "b", lVar "a"]
      @?= either' (either' void' (lVar "b")) (lVar "a")

-- | [ a , b , c ] -> a | ( b | c )
testSumFold3 :: TestTree
testSumFold3 =
  testCase "Fold 3 types" $
    foldWithSum [lVar "c", lVar "b", lVar "a"]
      @?= either' (either' (either' void' (lVar "c")) (lVar "b")) (lVar "a")

-- | TyDef to Kind Canonical representation - sums not folded - therefore we get constructor granularity. Might use in a different implementation for more granular errors.
lVar :: Text -> Type
lVar = Var . LocalRef

-- Property Tests
testRefl :: TestTree
testRefl = testProperty "Refl" reflTerm
  where
    reflTerm :: Property
    reflTerm = forAllShrink (arbitrary @Int) shrink (\a -> a == a)
