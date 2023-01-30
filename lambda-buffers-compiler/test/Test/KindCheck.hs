{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.KindCheck (test) where

import Control.Lens ((%~), (&), (.~))
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import LambdaBuffers.Compiler.KindCheck (
  check,
  foldWithProduct,
  foldWithSum,
 )
import LambdaBuffers.Compiler.KindCheck.Type (Type (App, Var))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

test :: TestTree
test = testGroup "Compiler tests" [testCheck, testFolds]

--------------------------------------------------------------------------------
-- Module tests

testCheck = testGroup "KindChecker Tests" [trivialKCTest, kcTestMaybe, kcTestFailing]

trivialKCTest =
  testCase "Empty CompInput should check." $
    check (P.CompilerInput []) @?= Right ()

kcTestMaybe =
  testCase "Maybe should pass." $
    check ci1 @?= Right ()

kcTestFailing =
  testCase "This should fail." $
    assertBool "Test should have failed." $
      check ci2 /= Right ()

esi = P.SourceInfo "Empty Info" (P.SourcePosition 0 0) (P.SourcePosition 0 1)

modMaybe =
  P.Module
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

ci1 :: P.CompilerInput
ci1 = P.CompilerInput {P.modules = [modMaybe]}

{- | Maybe = ...
   B a = B Maybe

 Should fail as B a defaults to B :: Type -> Type and Maybe is inferred as
 Type -> Type. This is an inconsistency failure.
-}
ci2 = ci1 & #modules .~ [addMod]
  where
    addMod =
      modMaybe
        & #typeDefs
          %~ ( <>
                [ -- B a = B Maybe
                  P.TyDef
                    { P.tyName = P.TyName "B" esi
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
                                        { P.constrName = P.ConstrName {P.name = "B", P.sourceInfo = esi}
                                        , P.product =
                                            P.TupleI $
                                              P.Tuple
                                                { P.fields =
                                                    [ P.TyRefI $
                                                        P.LocalI $
                                                          P.LocalRef
                                                            { P.tyName = P.TyName {P.name = "Maybe", P.sourceInfo = esi}
                                                            , P.sourceInfo = esi
                                                            }
                                                    ]
                                                , P.sourceInfo = esi
                                                }
                                        }
                                        :| []
                                  , sourceInfo = esi
                                  }
                          , P.sourceInfo = esi
                          }
                    , P.sourceInfo = esi
                    }
                ]
             )

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
