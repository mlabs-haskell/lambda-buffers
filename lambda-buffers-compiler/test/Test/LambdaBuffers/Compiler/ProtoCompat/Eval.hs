module Test.LambdaBuffers.Compiler.ProtoCompat.Eval (test) where

import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Test.LambdaBuffers.Compiler.ProtoCompat.Utils qualified as U
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "ProtoCompat Eval checks"
      [ fooCiTestCase
          "a -> a"
          (Just 1)
          (U.tv "a")
          "a"
      , fooCiTestCase
          "Prelude.Int8 -> opq"
          (Just 1)
          (U.fr ["Prelude"] "Int8")
          "opq"
      , fooCiTestCase
          "Maybe a -> (Nothing  | Just a)"
          (Just 1)
          (U.lr "Maybe" U.@ [U.tv "a"])
          "(Nothing  | Just a)"
      , fooCiTestCase
          "Prelude.Maybe a -> opq a"
          (Just 1)
          (U.fr ["Prelude"] "Maybe" U.@ [U.tv "a"])
          "(opq a)"
      , fooCiTestCase
          "Maybe Prelude.Int8 -> (Nothing  | Just Prelude.Int8)"
          (Just 1)
          (U.lr "Maybe" U.@ [U.fr ["Prelude"] "Int8"])
          "(Nothing  | Just Prelude.Int8)"
      , fooCiTestCase
          "Maybe Prelude.Int8 *-> (Nothing  | Just opq)"
          Nothing
          (U.lr "Maybe" U.@ [U.fr ["Prelude"] "Int8"])
          "(Nothing  | Just opq)"
      , fooCiTestCase
          "Maybe (Either Prelude.Int8 (Maybe Prelude.Int8)) -> (Nothing  | Just (Either Prelude.Int8 (Maybe Prelude.Int8)))"
          (Just 1)
          ( U.lr "Maybe"
              U.@ [ U.lr "Either"
                      U.@ [ U.fr ["Prelude"] "Int8"
                          , U.lr "Maybe"
                              U.@ [U.fr ["Prelude"] "Int8"]
                          ]
                  ]
          )
          "(Nothing  | Just (Either Prelude.Int8 (Maybe Prelude.Int8)))"
      , fooCiTestCase
          "Maybe (Either Prelude.Int8 (Maybe Prelude.Int8)) *-> (Nothing  | Just (Left opq | Right (Nothing  | Just opq)))"
          Nothing
          ( U.lr "Maybe"
              U.@ [ U.lr "Either"
                      U.@ [ U.fr ["Prelude"] "Int8"
                          , U.lr "Maybe" U.@ [U.fr ["Prelude"] "Int8"]
                          ]
                  ]
          )
          "(Nothing  | Just (Left opq | Right (Nothing  | Just opq)))"
      , fooCiTestCase
          "Foo Prelude.Int8 -> MkFoo (Prelude.Either Prelude.Int8 Prelude.Int8)"
          (Just 1)
          (U.lr "Foo" U.@ [U.fr ["Prelude"] "Int8"])
          "(MkFoo (Prelude.Either Prelude.Int8 Prelude.Int8))"
      , fooCiTestCase
          "Foo Prelude.Int8 *-> (MkFoo (opq opq opq))"
          Nothing
          (U.lr "Foo" U.@ [U.fr ["Prelude"] "Int8"])
          "(MkFoo (opq opq opq))"
      , fooCiTestCase
          "List (Foo a) -> (Nil  | Cons (Foo a) (List (Foo a)))"
          (Just 1)
          (U.lr "List" U.@ [U.lr "Foo" U.@ [U.tv "a"]])
          "(Nil  | Cons (Foo a) (List (Foo a)))"
      , fooCiTestCase
          "List (Foo a) 2-> (Nil  | Cons (MkFoo (Prelude.Either Prelude.Int8 a)) (Nil  | Cons (Foo a) (List (Foo a))))"
          (Just 2)
          (U.lr "List" U.@ [U.lr "Foo" U.@ [U.tv "a"]])
          "(Nil  | Cons (MkFoo (Prelude.Either Prelude.Int8 a)) (Nil  | Cons (Foo a) (List (Foo a))))"
      ]

fooCi :: PC.CompilerInput
fooCi =
  U.ci
    [ U.mod'prelude
    , U.mod
        ["Foo"]
        [ U.td "Foo" (U.abs ["a"] $ U.sum [("MkFoo", [U.fr ["Prelude"] "Either" U.@ [U.fr ["Prelude"] "Int8", U.tv "a"]])])
        , U.td'maybe
        , U.td'either
        , U.td'list
        ]
    ]

fooCiTestCase :: TestName -> Maybe Int -> PC.Ty -> String -> TestTree
fooCiTestCase title mayGas ty want = testCase title $ runTestFix mayGas fooCi (U.mn ["Foo"]) ty want

runTestFix :: Maybe Int -> PC.CompilerInput -> PC.ModuleName -> PC.Ty -> String -> Assertion
runTestFix mayGas ci mn ty want =
  let tydefs = PC.indexTyDefs ci
   in case fix mayGas (E.runEval' mn tydefs) (E.fromTy ty) of
        Left err -> assertFailure (show err)
        Right res -> do
          assertEqual "" want (show res)

fix :: Maybe Int -> (E.Ty -> Either e E.Ty) -> E.Ty -> Either e E.Ty
fix Nothing r x = case r x of
  Left err -> Left err
  Right x' -> if x == x' then Right x else fix Nothing r x'
fix (Just n) r x =
  if n <= 0
    then Right x
    else case r x of
      Left err -> Left err
      Right x' -> if x == x' then Right x else fix (Just (n - 1)) r x'
