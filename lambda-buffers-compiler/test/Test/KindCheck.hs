{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.KindCheck (test) where

import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import LambdaBuffers.Compiler.KindCheck (
  check,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds]

--------------------------------------------------------------------------------
-- Module tests

testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe]

trivialKCTest =
  testCase "Empty CompInput should check." $
    check (P.CompilerInput []) @?= Right ()

kcTestMaybe =
  testCase "Maybe should psss." $
    check ci1 @?= Right ()

ci1 :: P.CompilerInput
ci1 =
  P.CompilerInput
    [ P.Module
        { P.moduleName =
            P.ModuleName
              { P.parts = [P.ModuleNamePart "Module" esi]
              , P.sourceInfo = esi
              }
        , P.typeDefs =
            [ P.TyDef
                { P.tyName = P.TyName "Maybe" esi
                , P.tyAbs =
                    P.TyAbs
                      { P.tyArgs =
                          [ P.TyArg
                              { P.argName = P.VarName "a" esi
                              , P.argKind =
                                  P.Kind
                                    { P.kind = P.KindRef P.KType
                                    , P.sourceInfo = esi
                                    }
                              , P.sourceInfo = esi
                              }
                          ]
                      , P.tyBody =
                          P.SumI $
                            P.Sum
                              { constructors =
                                  P.Constructor
                                    { P.constrName = P.ConstrName {P.name = "Nothing", P.sourceInfo = esi}
                                    , P.product = P.TupleI $ P.Tuple {P.fields = [], P.sourceInfo = esi}
                                    }
                                    :| [ P.Constructor
                                          { P.constrName = P.ConstrName {P.name = "Just", P.sourceInfo = esi}
                                          , P.product =
                                              P.TupleI $
                                                P.Tuple
                                                  { P.fields =
                                                      [ P.TyVarI
                                                          ( P.TyVar
                                                              { P.varName =
                                                                  P.VarName
                                                                    { P.name = "a"
                                                                    , P.sourceInfo = esi
                                                                    }
                                                              , P.sourceInfo = esi
                                                              }
                                                          )
                                                      ]
                                                  , P.sourceInfo = esi
                                                  }
                                          }
                                       ]
                              , sourceInfo = esi
                              }
                      , P.sourceInfo = esi
                      }
                , P.sourceInfo = esi
                }
            ]
        , P.classDefs = mempty
        , P.instances = mempty
        , P.sourceInfo = esi
        }
    ]
  where
    esi :: P.SourceInfo -- empty Source Info
    esi = P.SourceInfo "Empty Info" (P.SourcePosition 0 0) (P.SourcePosition 0 1)

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

{-

runKC :: [TypeDefinition] -> Either KindCheckFailure [Kind]
runKC = runKindCheckEff . kindCheckType

t1 :: TestTree
t1 =
  testCase "No Definition, No Kinds" $
    runKC [] @?= Right []

t2 :: TestTree
t2 =
  testCase "Maybe has the correct Kind" $
    runKC [tdMaybe] @?= Right [Type :->: Type]

t3 :: TestTree
t3 =
  testCase "Maybe works correctly when used as a type" $
    runKC [tdT1, tdMaybe] @?= Right [Type :->: Type, Type :->: Type]

t4 :: TestTree
t4 =
  testCase "Maybe and a term containing a maybe work correctly" $
    runKC [tdT1, tdMaybe, tdT2] @?= Right [Type :->: Type, Type :->: Type, Type :->: Type]

t5 :: TestTree
t5 =
  testCase "Bad Type is caught and reported" $
    runKC [tdMaybe, tdBT0]
      @?= Left
        ( InferenceFailed
            ( TypeDefinition
                { _td'name = "T"
                , _td'variables = []
                , _td'sop = App (Var "Maybe") (Var "Maybe")
                }
            )
            (ImpossibleUnificationErr "Cannot unify: * = * \8594 *\n")
        )

--------------------------------------------------------------------------------
-- Manual type definitions.

tdMaybe :: TypeDefinition
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
tdT1 :: TypeDefinition
tdT1 =
  TypeDefinition
    { _td'name = "T"
    , _td'variables = ["b"]
    , _td'sop = Abs "b" $ App (Var "Maybe") (App (Var "Maybe") (Var "b"))
    }

-- T2 ~ T a = T Maybe (Maybe a)
tdT2 :: TypeDefinition
tdT2 =
  TypeDefinition
    { _td'name = "T2"
    , _td'variables = ["a"]
    , _td'sop = Abs "a" $ App (Var "T") (App (Var "Maybe") (Var "a"))
    }

-- T2 ~ T = T Maybe Maybe
tdBT0 :: TypeDefinition
tdBT0 =
  TypeDefinition
    { _td'name = "T"
    , _td'variables = []
    , _td'sop = App (Var "Maybe") (Var "Maybe")
    }
-}
