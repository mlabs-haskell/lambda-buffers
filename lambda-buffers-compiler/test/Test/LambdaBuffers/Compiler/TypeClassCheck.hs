module Test.LambdaBuffers.Compiler.TypeClassCheck (test) where

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck qualified as TC
import LambdaBuffers.Compiler.TypeClassCheck.Utils (moduleNameToMiniLogFilepath)
import Paths_lambda_buffers_compiler qualified as Path
import System.FilePath ((<.>), (</>))
import Test.LambdaBuffers.Compiler.ProtoCompat.Utils qualified as U
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "LambdaBuffers.Compiler.TypeClassCheck checks"
      [ succeeds "test1" test1
      , succeeds "test2" test2
      , succeeds "test3" test3
      , succeeds "test4" test4
      ]

{- Test 1 - should succeed

module Foo

import Prelude

sum Foo a b c = MkFoo (Either Int8 a)
  | MkBar (Maybe b) (List b)
  | MkBaz (Map b c)

derive Eq (Foo a b c)

derive Ord (Foo a b c)
-}
test1 :: PC.CompilerInput
test1 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
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
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"]]
    ]

{- Test 2 - should succeed

module Foo

import Prelude

sum Bar a = MkBar Bytes

derive Eq (Bar a)
derive Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
test2 :: PC.CompilerInput
test2 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td
            "Bar"
            ( U.abs ["a"] $
                U.sum
                  [ ("MkBar", [U.fr ["Prelude"] "Bytes"])
                  ]
            )
        , U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [ ("MkFoo", [U.lr "Bar" U.@ [U.tv "a"]])
                  ]
            )
        ]
        []
        []
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveEq (U.lr "Bar" U.@ [U.tv "a"])
        , deriveOrd (U.lr "Bar" U.@ [U.tv "a"])
        ]
        [["Prelude"]]
    ]

{- Test 3 - should succeed

module Foo

import Prelude

sum Bar a = MkBar Bytes

instance Eq (Bar a)
instance Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
test3 :: PC.CompilerInput
test3 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td
            "Bar"
            ( U.abs ["a"] $
                U.sum
                  [ ("MkBar", [U.fr ["Prelude"] "Bytes"])
                  ]
            )
        , U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [ ("MkFoo", [U.lr "Bar" U.@ [U.tv "a"]])
                  ]
            )
        ]
        []
        [ U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) []
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) []
        ]
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"]]
    ]

{- Test 4 - should succeed

module Foo

import Prelude

sum Bar a = MkBar Bytes

instance Eq a => Eq (Bar a)
instance Ord a => Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
test4 :: PC.CompilerInput
test4 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td
            "Bar"
            ( U.abs ["a"] $
                U.sum
                  [ ("MkBar", [U.fr ["Prelude"] "Bytes"])
                  ]
            )
        , U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [ ("MkFoo", [U.lr "Bar" U.@ [U.tv "a"]])
                  ]
            )
        ]
        []
        [ U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) [eqCstr (U.tv "a")]
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) [ordCstr (U.tv "a")]
        ]
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"]]
    ]

deriveEq :: PC.Ty -> PC.Derive
deriveEq = U.drv . eqCstr

eqCstr :: PC.Ty -> PC.Constraint
eqCstr = U.cstr (U.fcr ["Prelude"] "Eq")

deriveOrd :: PC.Ty -> PC.Derive
deriveOrd = U.drv . U.cstr (U.fcr ["Prelude"] "Ord")

ordCstr :: PC.Ty -> PC.Constraint
ordCstr = U.cstr (U.fcr ["Prelude"] "Ord")

succeeds :: TestName -> PC.CompilerInput -> TestTree
succeeds title ci = testCase title $ do
  case TC.runCheck' ci of
    (Left err, minilogs) -> do
      printMiniLogs minilogs
      assertFailure $ show ("Failed running type class checks with" :: String, err)
    (Right _, minilogs) -> do
      minilogGoldensDir <- Path.getDataFileName "data/minilog-goldens"
      for_
        (Map.toList minilogs)
        ( \(mn, got) -> do
            let fn = minilogGoldensDir </> title </> moduleNameToMiniLogFilepath mn <.> "pl"
            wanted <- readFile fn
            assertEqual ("Printed Prolog must match with " <> fn) wanted got
        )

printMiniLogs :: Map PC.ModuleName String -> IO ()
printMiniLogs mls = for_ (Map.toList mls) (\(mn, ml) -> print ("### Module" <> show (moduleNameToMiniLogFilepath mn)) >> print ml)
