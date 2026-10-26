module Test.LambdaBuffers.Runtime.PlutusTx.Eq (tests) where

import LambdaBuffers.Prelude.PlutusTx qualified as Lb
import PlutusTx.Builtins (
  bls12_381_G1_compressed_generator,
  bls12_381_G1_compressed_zero,
  bls12_381_G1_uncompress,
  bls12_381_G2_compressed_generator,
  bls12_381_G2_compressed_zero,
  bls12_381_G2_uncompress,
 )
import PlutusTx.Eq qualified as PlutusTxEq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "LB Prelude Eq instances (PlutusTx)"
    [ testCase "G1Element: an element equals itself" $
        assertBool "element == itself" ((PlutusTxEq.==) g1Element g1Element)
    , testCase "G1Element: distinct elements compare unequal" $
        assertBool "element /= zero element" ((PlutusTxEq./=) g1Element g1ZeroElement)
    , testCase "G2Element: an element equals itself" $
        assertBool "element == itself" ((PlutusTxEq.==) g2Element g2Element)
    , testCase "G2Element: distinct elements compare unequal" $
        assertBool "element /= zero element" ((PlutusTxEq./=) g2Element g2ZeroElement)
    ]

g1Element :: Lb.G1Element
g1Element = bls12_381_G1_uncompress bls12_381_G1_compressed_generator

g1ZeroElement :: Lb.G1Element
g1ZeroElement = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

g2Element :: Lb.G2Element
g2Element = bls12_381_G2_uncompress bls12_381_G2_compressed_generator

g2ZeroElement :: Lb.G2Element
g2ZeroElement = bls12_381_G2_uncompress bls12_381_G2_compressed_zero
