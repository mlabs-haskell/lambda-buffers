module Test.LambdaBuffers.Compiler.TypeClassCheck (test) where

import Data.Map qualified as Map
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck qualified as TC
import System.FilePath ((<.>), (</>))
import Test.LambdaBuffers.Compiler.ProtoCompat.Utils qualified as U
import Test.LambdaBuffers.Compiler.Utils.Golden qualified as Golden
import Test.Tasty (TestName, TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "LambdaBuffers.Compiler.TypeClassCheck checks"
    [ testGroup
        "Should succeed"
        [ succeeds "test1" test1
        , succeeds "test2" test2
        , succeeds "test3" test3
        , succeeds "test4" test4
        , succeeds "test5" test5
        , succeeds "test6" test6
        ]
    , testGroup
        "Should fail"
        [ fails "derive_opaque_1" testDeriveOpaque1
        , fails "derive_opaque_2" testDeriveOpaque2
        , fails "missing_rule_1" testMissingRule1
        , fails "missing_rule_2" testMissingRule2
        , fails "missing_rule_3" testMissingRule3
        , fails "overlapping_rules_1" testOverlaps1
        ]
    , testGroup
        "Cycled goals should succeed"
        [ succeeds "cycled_goals_1" testCycledGoals1
        , succeeds "cycled_goals_2" testCycledGoals2
        ]
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

{- Test 5 - should succeed

module Bar

import Prelude

opaque Bar a

instance Eq a => Eq (Bar a)
instance Ord a => Ord (Bar a)

module Baz

import Prelude

opaque Baz a

instance Eq a => Eq (Baz a)

module Foo

import Bar
import Baz
import Prelude

sum Foo a b c = MkFoo (Bar a) (Baz Int8) b c

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
test5 :: PC.CompilerInput
test5 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Bar"]
        [ U.td
            "Bar"
            (U.abs ["a"] U.opq)
        ]
        []
        [ U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) [eqCstr (U.tv "a")]
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) [ordCstr (U.tv "a")]
        ]
        []
        [["Prelude"]]
    , U.mod'
        ["Baz"]
        [ U.td
            "Baz"
            (U.abs ["a"] U.opq)
        ]
        []
        [ U.inst' (eqCstr (U.lr "Baz" U.@ [U.tv "a"])) [eqCstr (U.tv "a")]
        , U.inst' (ordCstr (U.lr "Baz" U.@ [U.tv "a"])) [ordCstr (U.tv "a")]
        ]
        []
        [["Prelude"]]
    , U.mod'
        ["Foo"]
        [ U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [
                    ( "MkFoo"
                    ,
                      [ U.fr ["Bar"] "Bar" U.@ [U.tv "a"]
                      , U.fr ["Baz"] "Baz" U.@ [U.fr ["Prelude"] "Int8"]
                      , U.tv "b"
                      , U.tv "c"
                      ]
                    )
                  ]
            )
        ]
        []
        []
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"], ["Baz"], ["Bar"]]
    ]

{- Test 6 - should succeed

module Bar

import Prelude

opaque Bar a

instance Eq a => Eq (Bar a)
instance Ord a => Ord (Bar a)

module Baz

import Prelude
import Bar

sum Baz a = MkBaz (Bar a)

derive Eq (Baz a)

module Foo

import Baz
import Prelude

sum Foo a b c = MkFoo (Baz a) b c

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
test6 :: PC.CompilerInput
test6 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Bar"]
        [ U.td
            "Bar"
            (U.abs ["a"] U.opq)
        ]
        []
        [ U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) [eqCstr (U.tv "a")]
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) [ordCstr (U.tv "a")]
        ]
        []
        [["Prelude"]]
    , U.mod'
        ["Baz"]
        [ U.td
            "Baz"
            ( U.abs ["a"] $
                U.sum
                  [("MkBaz", [U.fr ["Bar"] "Bar" U.@ [U.tv "a"]])]
            )
        ]
        []
        []
        [ deriveEq (U.lr "Baz" U.@ [U.tv "a"])
        , deriveOrd (U.lr "Baz" U.@ [U.tv "a"])
        ]
        [["Prelude"], ["Bar"]]
    , U.mod'
        ["Foo"]
        [ U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [
                    ( "MkFoo"
                    ,
                      [ U.fr ["Baz"] "Baz" U.@ [U.tv "a"]
                      , U.tv "b"
                      , U.tv "c"
                      ]
                    )
                  ]
            )
        ]
        []
        []
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"], ["Baz"]]
    ]

{- Test Derive opaques 1 - should fail

module Foo

import Prelude

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)

opaque Bar a

derive Eq (Bar a)
-}
testDeriveOpaque1 :: PC.CompilerInput
testDeriveOpaque1 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Bar" (U.abs ["a"] U.opq)
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

{- Test Derive opaque 2 - should fail

module Foo

import Prelude

opaque Bar a

derive Eq (Bar a)
-}
testDeriveOpaque2 :: PC.CompilerInput
testDeriveOpaque2 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [U.td "Bar" (U.abs ["a"] U.opq)]
        []
        []
        [deriveEq (U.lr "Bar" U.@ [U.tv "a"])]
        [["Prelude"]]
    ]

{- Test Cycled goals 1 - a cycle should succeed

module Foo

import Prelude

opaque Bar a

instance Eq (Baz a) => Eq (Bar a)
instance (Eq a) => Ord (Bar a)

opaque Baz a

instance Eq (Bar a) => Eq (Baz a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
testCycledGoals1 :: PC.CompilerInput
testCycledGoals1 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Bar" (U.abs ["a"] U.opq)
        , U.td "Baz" (U.abs ["a"] U.opq)
        , U.td
            "Foo"
            ( U.abs ["a", "b", "c"] $
                U.sum
                  [ ("MkFoo", [U.lr "Bar" U.@ [U.tv "a"]])
                  ]
            )
        ]
        []
        [ U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) [eqCstr (U.lr "Baz" U.@ [U.tv "a"])]
        , U.inst' (eqCstr (U.lr "Baz" U.@ [U.tv "a"])) [eqCstr (U.lr "Bar" U.@ [U.tv "a"])]
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) [eqCstr (U.tv "a")]
        ]
        [ deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])
        ]
        [["Prelude"]]
    ]

{- Test Cycled goals 1 - a cycle should succeed on recursive types

module Foo

import Prelude

prod Bar a = Bar a

derive Eq (Bar a)

record Baz a b = { baz : Baz a b}

derive Eq (Baz a b)

sum Foo a b = FooA (Foo a b) | FooB (Foo a b)

derive Eq (Foo a b)
derive Ord (Foo a b)
-}
testCycledGoals2 :: PC.CompilerInput
testCycledGoals2 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Bar" (U.abs ["a"] $ U.prod' [U.lr "Bar" U.@ [U.tv "a"]])
        , U.td "Baz" (U.abs ["a", "b"] $ U.recrd [("baz", U.lr "Baz" U.@ [U.tv "a", U.tv "b"])])
        , U.td
            "Foo"
            ( U.abs ["a", "b"] $
                U.sum
                  [ ("FooA", [U.lr "Foo" U.@ [U.tv "a", U.tv "b"]])
                  , ("FooB", [U.lr "Foo" U.@ [U.tv "a", U.tv "b"]])
                  ]
            )
        ]
        []
        []
        [ deriveEq (U.lr "Bar" U.@ [U.tv "a"])
        , deriveOrd (U.lr "Bar" U.@ [U.tv "a"])
        , deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b"])
        , deriveOrd (U.lr "Foo" U.@ [U.tv "a", U.tv "b"])
        , deriveEq (U.lr "Baz" U.@ [U.tv "a", U.tv "b"])
        , deriveOrd (U.lr "Baz" U.@ [U.tv "a", U.tv "b"])
        ]
        [["Prelude"]]
    ]

{- Test Missing Rule1 - should fail

module Foo

import Prelude

opaque Bar a

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
testMissingRule1 :: PC.CompilerInput
testMissingRule1 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Bar" (U.abs ["a"] U.opq)
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
        ]
        [["Prelude"]]
    ]

{- Test Missing Rule2 - should fail

module Foo

import Prelude

opaque Baz a

sum Bar a = MkBar (Baz a)

derive Eq (Bar a)
derive Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
testMissingRule2 :: PC.CompilerInput
testMissingRule2 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Baz" (U.abs ["a"] U.opq)
        , U.td
            "Bar"
            ( U.abs ["a"] $
                U.sum
                  [ ("MkBar", [U.lr "Baz" U.@ [U.tv "a"]])
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

{- Test Missing Rule 3 - should fail

module Baz

import Prelude

opaque Baz a

module Foo

sum Bar a = MkBar (Baz a)

derive Eq (Bar a)
derive Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
testMissingRule3 :: PC.CompilerInput
testMissingRule3 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td
            "Bar"
            ( U.abs ["a"] $
                U.sum
                  [ ("MkBar", [U.fr ["Baz"] "Baz" U.@ [U.tv "a"]])
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
        [["Prelude"], ["Baz"]]
    , U.mod'
        ["Baz"]
        [U.td "Baz" (U.abs ["a"] U.opq)]
        []
        []
        []
        [["Prelude"]]
    ]

{- Test Overlaps 1 - overlapping rules should fail

module Foo

import Prelude

opaque Bar a

instance Eq (Bar a)
instance Eq a => Eq (Bar a)
instance Ord (Bar a)

sum Foo a b c = MkFoo (Bar a)

derive Eq (Foo a b c)
derive Ord (Foo a b c)
-}
testOverlaps1 :: PC.CompilerInput
testOverlaps1 =
  U.ci
    [ U.mod'preludeO
    , U.mod'
        ["Foo"]
        [ U.td "Bar" (U.abs ["a"] U.opq)
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
        , U.inst' (eqCstr (U.lr "Bar" U.@ [U.tv "a"])) []
        , U.inst' (ordCstr (U.lr "Bar" U.@ [U.tv "a"])) []
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
succeeds title ci =
  Golden.succeeds
    goldensDir
    (\baseFp -> let fn = baseFp <.> "pl" in (,) <$> readFile fn <*> pure fn)
    writeFile
    title
    ( case TC.runCheck' ci of
        (Left err, _printed) -> Left err
        (Right _, printed) -> Right $ Map.mapKeys Just printed
    )

fails :: TestName -> PC.CompilerInput -> TestTree
fails title ci =
  Golden.fails
    goldensDir
    (\tdir -> let fn = tdir </> "compiler_error" <.> "textproto" in (,) <$> (PbText.readMessageOrDie <$> Text.readFile fn) <*> pure fn)
    (\otherFn gotErr -> Text.writeFile otherFn (Text.pack . show $ PbText.pprintMessage gotErr))
    title
    (fst $ TC.runCheck' ci)

goldensDir :: FilePath
goldensDir = "data/typeclasscheck-goldens"
