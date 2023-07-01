module Test.LambdaBuffers.Compiler.ClassClosure (
  tests,
) where

import Control.Lens ((^.))
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import LambdaBuffers.ProtoCompat qualified as PC
import Test.LambdaBuffers.ProtoCompat.Utils qualified as U
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (Monoid (mconcat), Show (show), zip, ($), (.), (<$>), (<>))

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Compiler.ClassClosure checks"
    [ testGroup
        "All classes in closure and related rules should be removed"
        [ classClosureTest "test1" [(["Prelude"], "Ord")] [(["Prelude"], "Eq"), (["Prelude"], "Ord")] test1
        ]
    ]

classClosureTest :: TestName -> [([Text], Text)] -> [([Text], Text)] -> (PC.CompilerInput, PC.CompilerInput) -> TestTree
classClosureTest title cls clsWant (ciIn, ciWant) =
  let
    classRels = PC.indexClassRelations ciIn
    cls' = Set.fromList [U.qcln' (U.mn mn) clN | (mn, clN) <- cls]
    clsWant' = Set.fromList [U.qcln' (U.mn mn) clN | (mn, clN) <- clsWant]
    cls'' = PC.classClosure classRels cls'
    clsWant'' = PC.classClosure classRels clsWant'
    ciGot = PC.CompilerInput $ PC.filterClassInModule cls'' <$> ciIn ^. #modules
   in
    testGroup
      title
      $ testCase "Class closure should be" (cls'' @?= clsWant'')
        : ( [ testGroup
              (PC.prettyModuleName (mGot' ^. #moduleName))
              [ testCase "Class definitions should match" $ (mGot' ^. #classDefs) @?= (mWant ^. #classDefs)
              , testCase "Instances should match" $ (mGot' ^. #instances) @?= (mWant ^. #instances)
              , testCase "Derives should match" $ (mGot' ^. #derives) @?= (mWant ^. #derives)
              ]
            | (mWant, mGot') <- zip (toList $ ciWant ^. #modules) (toList $ ciGot ^. #modules)
            ]
          )

test1 :: (PC.CompilerInput, PC.CompilerInput)
test1 =
  ( U.ci
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
  , U.ci
      [ U.mod'preludeO {PC.classDefs = Map.empty, PC.instances = []}
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
          [deriveEq (U.lr "Foo" U.@ [U.tv "a", U.tv "b", U.tv "c"])]
          [["Prelude"]]
      ]
  )

deriveEq :: PC.Ty -> PC.Derive
deriveEq = U.drv . eqCstr

eqCstr :: PC.Ty -> PC.Constraint
eqCstr = U.cstr (U.fcr ["Prelude"] "Eq")

deriveOrd :: PC.Ty -> PC.Derive
deriveOrd = U.drv . ordCstr

ordCstr :: PC.Ty -> PC.Constraint
ordCstr = U.cstr (U.fcr ["Prelude"] "Ord")
