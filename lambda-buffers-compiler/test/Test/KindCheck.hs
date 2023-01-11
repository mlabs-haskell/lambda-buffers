module Test.KindCheck (test) where

import LambdaBuffers.Compiler.KindCheck
import LambdaBuffers.Compiler.KindCheck.Inference
import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test = testGroup "KindChecker Tests" [t1, t2, t3, t4]

runKC :: [TypeDefinition] -> Either KindCheckFailure [Kind]
runKC = runKindCheckEff . kindCheckType

t1 =
  testCase "No Definition, No Kinds." $
    runKC [] @?= Right []

t2 =
  testCase "Maybe has the correct Kind." $
    runKC [tdMaybe] @?= Right [Type :->: Type]

t3 =
  testCase "Maybe works correctly when used as a type." $
    runKC [tdT1, tdMaybe] @?= Right [Type :->: Type, Type :->: Type]

t4 =
  testCase "Maybe and a term containing a maybe work correctly." $
    runKC [tdT1, tdMaybe, tdT2] @?= Right [Type :->: Type, Type :->: Type, Type :->: Type]

--------------------------------------------------------------------------------
-- Manual type definitions.

tdMaybe =
  TypeDefinition
    { _td'name = "Maybe"
    , _td'variables = ["a"]
    , _td'sop =
        Abs "a" $
          App
            (App (Var "Either") (Var "()"))
            (Var "a")
    }

-- T1 ~ T a = T Maybe (Maybe a)
tdT1 =
  TypeDefinition
    { _td'name = "T"
    , _td'variables = ["b"]
    , _td'sop = Abs "b" $ App (Var "Maybe") (App (Var "Maybe") (Var "b"))
    }

-- T2 ~ T a = T Maybe (Maybe a)
tdT2 =
  TypeDefinition
    { _td'name = "T2"
    , _td'variables = ["a"]
    , _td'sop = Abs "a" $ App (Var "T") (App (Var "Maybe") (Var "a"))
    }
