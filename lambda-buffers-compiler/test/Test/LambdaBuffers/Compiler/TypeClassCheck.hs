module Test.LambdaBuffers.Compiler.TypeClassCheck (test) where

import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck qualified as TC
import Test.LambdaBuffers.Compiler.ProtoCompat.Utils qualified as U
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "LambdaBuffers.Compiler.TypeClassCheck checks"
      [ succeeds "fooCi" fooCi
      ]

fooCi :: PC.CompilerInput
fooCi =
  U.ci
    [ U.mod'preludeO
    , fooMod
    ]

fooMod :: PC.Module
fooMod =
  U.mod'
    ["Foo"]
    [ U.td
        "Foo"
        ( U.abs ["a", "b", "c"] $
            U.sum
              [ ("MkFoo", [U.fr ["Prelude"] "Either" U.@ [U.fr ["Prelude"] "Int8", U.tv "a"]])
              , ("MkBar", [U.fr ["Prelude"] "Maybe" U.@ [U.tv "b"], U.fr ["Prelude"] "List" U.@ [U.tv "b"]])
              , ("MkBaz", [U.fr ["Prelude"] "Map" U.@ [U.tv "b", U.tv "c"]])
              ]
        )
    ]
    []
    []
    [ U.drv $ U.cstr (U.fcr ["Prelude"] "Eq") (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
    , U.drv $ U.cstr (U.fcr ["Prelude"] "Ord") (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
    ]
    [["Prelude"]]

succeeds :: TestName -> PC.CompilerInput -> TestTree
succeeds title ci = testCase title $ do
  case TC.runCheck ci of
    Left err -> assertFailure $ show ("Failed running type class checks with" :: String, err)
    Right _ -> return ()
