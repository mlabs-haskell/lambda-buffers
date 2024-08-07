module Test.LambdaBuffers.Compiler.LamTy (test) where

import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Test.LambdaBuffers.ProtoCompat.Utils qualified as U
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "LambdaBuffers.Compiler.LamTy checks"
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
      , fooCiTestCase
          "Bar 1-> Prelude.Int8"
          (Just 1)
          (U.lr "Bar")
          "Prelude.Int8"
      , fooCiTestCase
          "Baz 1-> Prelude.Int8"
          (Just 1)
          (U.lr "Baz")
          "{baz : Prelude.Int8}"
      , fooCiTestCase
          "Beez Text 1-> {baz : Prelude.Int8,beez : Prelude.Text}"
          (Just 1)
          (U.lr "Beez" U.@ [U.fr ["Prelude"] "Text"])
          "{baz : Prelude.Int8,beez : Prelude.Text}"
      , fooCiTestCase
          "Beer Text 1-> Prelude.Int8 Prelude.Text"
          (Just 1)
          (U.lr "Beer" U.@ [U.fr ["Prelude"] "Text"])
          "Prelude.Int8 Prelude.Text"
      , fooCiTestCase
          "Phantom a 1-> Prelude.Int8"
          (Just 1)
          (U.lr "Phantom" U.@ [U.tv "a"])
          "Prelude.Int8"
      , fooCiTestCase
          "Phantom a *-> opq"
          Nothing
          (U.lr "Phantom" U.@ [U.tv "a"])
          "opq"
      , fooCiTestCase
          "RecursiveA a 1-> opq"
          (Just 1)
          (U.lr "RecursiveA" U.@ [U.tv "a"])
          "Prelude.Int8 (RecursiveA a)"
      ]

fooCi :: PC.CompilerInput
fooCi =
  U.ci
    [ U.mod'preludeO
    , U.mod
        ["Foo"]
        [ U.td "Foo" (U.abs ["a"] $ U.sum [("MkFoo", [U.fr ["Prelude"] "Either" U.@ [U.fr ["Prelude"] "Int8", U.tv "a"]])])
        , U.td "Baz" (U.abs [] $ U.recrd [("baz", U.fr ["Prelude"] "Int8")])
        , U.td "Bar" (U.abs [] $ U.prod' [U.fr ["Prelude"] "Int8"])
        , U.td
            "Beez"
            ( U.abs ["a"] $
                U.recrd
                  [ ("baz", U.fr ["Prelude"] "Int8")
                  , ("beez", U.tv "a")
                  ]
            )
        , U.td "Beer" (U.abs ["a"] $ U.prod' [U.fr ["Prelude"] "Int8", U.tv "a"])
        , U.td "Phantom" (U.abs ["a"] $ U.prod' [U.fr ["Prelude"] "Int8"])
        , U.td "RecursiveA" (U.abs ["a"] $ U.prod' [U.fr ["Prelude"] "Int8", U.lr "RecursiveA" U.@ [U.tv "a"]])
        , U.td'maybe
        , U.td'either
        , U.td'list
        ]
    ]

fooCiTestCase :: TestName -> Maybe Int -> PC.Ty -> String -> TestTree
fooCiTestCase title mayGas ty want = testCase title $ runTestFix mayGas fooCi (U.mn ["Foo"]) ty want

runTestFix :: Maybe Int -> PC.CompilerInput -> PC.ModuleName -> PC.Ty -> String -> Assertion
runTestFix mayGas ci mn ty want = case LT.runEvalWithGas mayGas ci mn ty of
  Left err -> assertFailure (show err)
  Right res -> do
    assertEqual "" want (show res)
