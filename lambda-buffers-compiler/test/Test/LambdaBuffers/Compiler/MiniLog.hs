module Test.LambdaBuffers.Compiler.MiniLog (test) where

import Control.Monad (void)
import Data.Map qualified as Map
import LambdaBuffers.Compiler.MiniLog (Clause, MiniLogError (MissingGoalError, OverlappingClausesError), Term (Atom, Struct, Var), VarName, (@<=))
import LambdaBuffers.Compiler.MiniLog.UniFdSolver (solve)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "MiniLog checks"
      [shouldSolve, shouldFailToSolve]

shouldFailToSolve :: TestTree
shouldFailToSolve =
  testGroup
    "Should fail to solve"
    [ testCase "?- animal(X)." $
        failsWith
          greekKnowledge
          [Struct "animal" [Var "X"]]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "?- animal(Y)." $
        failsWith
          greekKnowledge
          [Struct "animal" [Var "Y"]]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "?- human(X)." $
        failsWith
          greekKnowledge
          [Struct "human" [Var "X"]]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "?- human(X),human(Y)." $
        failsWith
          greekKnowledge
          [Struct "human" [Var "X"], Struct "human" [Var "Y"]]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "?- animal(aristotle)." $
        failsWith
          greekKnowledge
          [Struct "animal" [Atom "aristotle"]]
          (MissingGoalError (Struct "human" [Atom "aristotle"]))
    , testCase "human(plato).;human(plato). ?- human(plato)." $
        failsWith
          (platoIsHuman : greekKnowledge)
          [Struct "human" [Atom "plato"]]
          (OverlappingClausesError [platoIsHuman, platoIsHuman])
    , testCase "human(plato).;human(plato). ?- animal(plato)." $
        failsWith
          (platoIsHuman : greekKnowledge)
          [Struct "animal" [Atom "plato"]]
          (OverlappingClausesError [platoIsHuman, platoIsHuman])
    , testCase " ?- ancestor(vlado, nenad). %% NOTE(bladyjoker): Unsupported feature." $
        failsWith
          familyKnowledge
          [Struct "ancestor" [Atom "vlado", Atom "nenad"]]
          (OverlappingClausesError [ancestorIsParent, ancestorTransitive])
    , testCase " ?- eq(maybe(A))." $
        failsWith
          eqTypeClassKnowledge
          [ Struct
              "eq"
              [ Struct "maybe" [Var "A"]
              ]
          ]
          (OverlappingClausesError eqTypeClassKnowledge)
    ]

shouldSolve :: TestTree
shouldSolve =
  testGroup
    "Should be solvable"
    [ testCase "?- animal(socrates)." $
        succeedsWith
          greekKnowledge
          [Struct "animal" [Atom "socrates"]]
          mempty
    , testCase "?- animal(plato)." $
        succeedsWith
          greekKnowledge
          [Struct "animal" [Atom "plato"]]
          mempty
    , testCase "?- human(socrates)." $
        succeedsWith
          greekKnowledge
          [Struct "human" [Atom "socrates"]]
          mempty
    , testCase "?- human(plato)." $
        succeedsWith
          greekKnowledge
          [Struct "human" [Atom "plato"]]
          mempty
    , testCase "?- animal(socrates),animal(plato)." $
        succeedsWith
          greekKnowledge
          [Struct "animal" [Atom "socrates"], Struct "animal" [Atom "plato"]]
          mempty
    , testCase " ?- parent(slavka, nenad)." $
        succeedsWith
          familyKnowledge
          [Struct "parent" [Atom "slavka", Atom "nenad"]]
          mempty
    , testCase " ?- eq(maybe(var('A')))." $
        succeedsWith
          eqTypeClassKnowledge
          [Struct "eq" [Struct "maybe" [Struct "var" [Atom "A"]]]]
          mempty
    , testCase " ?- eq(var('A'))." $
        succeedsWith
          eqTypeClassKnowledge
          [Struct "eq" [Struct "var" [Atom "A"]]]
          mempty
    , testCase " ?- eq(var(A))." $
        succeedsWith
          eqTypeClassKnowledge
          [Struct "eq" [Struct "var" [Var "A"]]]
          mempty
    , testCase " ?- grandparent(vlado, nenad)." $
        succeedsWith
          familyKnowledge
          [Struct "grandparent" [Atom "vlado", Atom "nenad"]]
          mempty
    , testCase " ?- grandparent(vlado, X)." $
        succeedsWith
          familyKnowledge
          [Struct "grandparent" [Atom "vlado", Var "X"]]
          [("X", Atom "nenad")]
    , testCase " ?- parent(zdravka, X)." $
        succeedsWith
          familyKnowledge
          [Struct "parent" [Atom "zdravka", Var "X"]]
          [("X", Atom "slavka")]
    , testCase " ?- ggrandparent(dusan, X)." $
        succeedsWith
          familyKnowledge
          [Struct "ggrandparent" [Atom "dusan", Var "X"]]
          [("X", Atom "nenad")]
    , testCase " ?- ggrandparent2(dusan, X)." $
        succeedsWith
          familyKnowledge
          [Struct "ggrandparent2" [Atom "mitar", Var "X"]]
          [("X", Atom "nenad")]
    , testCase " ?- eq(int)." $
        succeedsWith
          eqTypeClassKnowledge
          [Struct "eq" [Atom "int"]]
          mempty
    ]

type TestTerm = Term String String
type TestClause = Clause String String

platoIsHuman :: TestClause
platoIsHuman = Struct "human" [Atom "plato"] @<= []

socratesIsHuman :: TestClause
socratesIsHuman = Struct "human" [Atom "socrates"] @<= []

greekKnowledge :: [TestClause]
greekKnowledge =
  [ Struct "animal" [Var "X"] @<= [Struct "human" [Var "X"]]
  , socratesIsHuman
  , platoIsHuman
  ]

ancestorIsParent :: TestClause
ancestorIsParent =
  Struct "ancestor" [Var "Anc", Var "Dec"]
    @<= [ Struct "parent" [Var "Anc", Var "Dec"]
        ]

ancestorTransitive :: TestClause
ancestorTransitive =
  Struct "ancestor" [Var "Anc", Var "X"]
    @<= [ Struct "parent" [Var "Anc", Var "X"]
        , Struct "ancestor" [Var "X", Var "Dec"]
        ]
familyKnowledge :: [TestClause]
familyKnowledge =
  [ Struct "parent" [Atom "slavka", Atom "nenad"] @<= []
  , Struct "parent" [Atom "zoran", Atom "nenad"] @<= []
  , Struct "parent" [Atom "vlado", Atom "zoran"] @<= []
  , Struct "parent" [Atom "ljeposava", Atom "zoran"] @<= []
  , Struct "parent" [Atom "zdravka", Atom "slavka"] @<= []
  , Struct "parent" [Atom "slavko", Atom "slavka"] @<= []
  , Struct "parent" [Atom "mitar", Atom "ljeposava"] @<= []
  , Struct "parent" [Atom "dusan", Atom "vlado"] @<= []
  , ancestorIsParent
  , ancestorTransitive
  , Struct "grandparent" [Var "Gp", Var "Gc"]
      @<= [ Struct "parent" [Var "Gp", Var "P"]
          , Struct "parent" [Var "P", Var "Gc"]
          ]
  , Struct "ggrandparent" [Var "Ggp", Var "Ggc"]
      @<= [ Struct "parent" [Var "Ggp", Var "Gp"]
          , Struct "parent" [Var "Gp", Var "P"]
          , Struct "parent" [Var "P", Var "Ggc"]
          ]
  , Struct "ggrandparent2" [Var "Ggp", Var "Ggc"]
      @<= [ Struct "grandparent" [Var "Ggp", Var "P"]
          , Struct "parent" [Var "P", Var "Ggc"]
          ]
  ]

eqTypeClass :: TestTerm -> TestTerm
eqTypeClass ty = Struct "eq" [ty]

eqTypeClassKnowledge :: [TestClause]
eqTypeClassKnowledge =
  [ eqTypeClass (Atom "int") @<= []
  , eqTypeClass (Atom "bytes") @<= []
  , eqTypeClass (Struct "maybe" [Var "A"]) @<= [eqTypeClass (Var "A")]
  , eqTypeClass (Struct "either" [Var "A", Var "B"]) @<= [eqTypeClass (Var "A"), eqTypeClass (Var "B")]
  , eqTypeClass (Struct "var" [Var "X"]) @<= []
  ]

succeedsWith :: [TestClause] -> [TestTerm] -> [(VarName, TestTerm)] -> Assertion
succeedsWith clauses goals wanted =
  let (errOrRes, logs) = solve clauses goals
   in case errOrRes of
        Left err -> do
          void $ print `traverse` logs
          assertFailure $ show err
        Right got -> assertEqual "Solutions should match" got (Map.fromList wanted)

failsWith :: [TestClause] -> [TestTerm] -> MiniLogError String String -> Assertion
failsWith clauses goals wanted =
  let (errOrRes, logs) = solve clauses goals
   in case errOrRes of
        Left err -> assertEqual "Errors should match" err wanted
        Right sols -> do
          void $ print `traverse` logs
          assertFailure $ show ("Wanted an error but got" :: String, sols)
