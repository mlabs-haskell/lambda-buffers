module Test.LambdaBuffers.Compiler.MiniLog (test) where

import Control.Monad (void)
import Data.Map qualified as Map
import LambdaBuffers.Compiler.MiniLog (Clause, MiniLogError (CycledGoalsError, MissingClauseError, OverlappingClausesError), Term (Atom, Struct, Var), VarName, (@<=))
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
    [ testCase "greek ?- animal(X). % overlaps on human(plato|socrates)" $
        failsWith
          greekKnowledge
          [animal (Var "X")]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "greek ?- animal(Y). % overlaps on human(plato|socrates)" $
        failsWith
          greekKnowledge
          [animal (Var "Y")]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "greek ?- human(X). % overlaps on human(plato|socrates)" $
        failsWith
          greekKnowledge
          [human (Var "X")]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "greek ?- human(X),human(Y). % overlaps on human(plato|socrates)" $
        failsWith
          greekKnowledge
          [human (Var "X"), human (Var "Y")]
          (OverlappingClausesError [socratesIsHuman, platoIsHuman])
    , testCase "greek ?- animal(aristotle). % missing goal human(ariostotle)" $
        failsWith
          greekKnowledge
          [animal (Atom "aristotle")]
          (MissingClauseError (human (Atom "aristotle")))
    , testCase "human(plato).;human(plato). greek ?- human(plato). % overlaps on human(plato|socrates)" $
        failsWith
          (platoIsHuman : greekKnowledge)
          [human (Atom "plato")]
          (OverlappingClausesError [platoIsHuman, platoIsHuman])
    , testCase "human(plato).;human(plato). greek ?- animal(plato).  % overlaps on human(plato|plato)" $
        failsWith
          (platoIsHuman : greekKnowledge)
          [animal (Atom "plato")]
          (OverlappingClausesError [platoIsHuman, platoIsHuman])
    , testCase " family ?- ancestor(vlado, nenad). % overlaps on ancestor rules NOTE(bladyjoker): Could be supported." $
        failsWith
          familyKnowledge
          [ancestor (Atom "vlado") (Atom "nenad")]
          (OverlappingClausesError [ancestorIsParent, ancestorTransitive])
    , testCase "typeclasses ?- eq(maybe(X)). % overlaps on all eq(X)" $
        failsWith
          eqTypeClassKnowledge
          [eq (Struct "maybe" [Var "X"])]
          (OverlappingClausesError eqTypeClassKnowledge)
    , testCase "greek ?- animal(plato), animal(socrates), human(plato), human(socrates), animal(aristotle) % missing goal human(aristotle)" $
        failsWith
          greekKnowledge
          [animal (Atom "plato"), animal (Atom "socrates"), human (Atom "plato"), human (Atom "socrates"), animal (Atom "aristotle")]
          (MissingClauseError (human (Atom "aristotle")))
    , testCase "cyclic ?- eq(beep(int))." $
        failsWith
          cycleKnowledge
          [eq (Struct "beep" [Atom "int"])]
          (CycledGoalsError [eq (Struct "beep" [Atom "int"])])
    , testCase "cyclic ?- eq(foo(int))." $
        failsWith
          cycleKnowledge
          [eq (Struct "foo" [Atom "int"])]
          (CycledGoalsError [eq (Struct "foo" [Atom "int"]), eq (Struct "bar" [Atom "int"]), eq (Struct "baz" [Atom "int"])])
    ]

shouldSolve :: TestTree
shouldSolve =
  testGroup
    "Should be solvable"
    [ testCase "greek ?- animal(socrates)." $
        succeedsWith
          greekKnowledge
          [animal (Atom "socrates")]
          mempty
    , testCase "greek ?- animal(plato)." $
        succeedsWith
          greekKnowledge
          [animal (Atom "plato")]
          mempty
    , testCase "greek ?- human(socrates)." $
        succeedsWith
          greekKnowledge
          [human (Atom "socrates")]
          mempty
    , testCase "greek ?- human(plato)." $
        succeedsWith
          greekKnowledge
          [human (Atom "plato")]
          mempty
    , testCase "greek ?- animal(socrates),animal(plato)." $
        succeedsWith
          greekKnowledge
          [animal (Atom "socrates"), animal (Atom "plato")]
          mempty
    , testCase "family ?- parent(slavka, nenad)." $
        succeedsWith
          familyKnowledge
          [parent (Atom "slavka") (Atom "nenad")]
          mempty
    , testCase "typeclasses ?- eq(maybe(var('A')))." $
        succeedsWith
          eqTypeClassKnowledge
          [eq (Struct "maybe" [Struct "var" [Atom "A"]])]
          mempty
    , testCase "typeclasses ?- eq(var('A'))." $
        succeedsWith
          eqTypeClassKnowledge
          [eq (Struct "var" [Atom "A"])]
          mempty
    , testCase "typeclasses ?- eq(var(A))." $
        succeedsWith
          eqTypeClassKnowledge
          [eq (Struct "var" [Var "A"])]
          [("A", Var "-9223372036854775808")]
    , testCase "family ?- grandparent(vlado, nenad)." $
        succeedsWith
          familyKnowledge
          [grandparent (Atom "vlado") (Atom "nenad")]
          mempty
    , testCase "family ?- grandparent(vlado, X)." $
        succeedsWith
          familyKnowledge
          [grandparent (Atom "vlado") (Var "X")]
          [("X", Atom "nenad")]
    , testCase "family ?- parent(zdravka, X)." $
        succeedsWith
          familyKnowledge
          [parent (Atom "zdravka") (Var "X")]
          [("X", Atom "slavka")]
    , testCase "family ?- ggrandparent(dusan, X)." $
        succeedsWith
          familyKnowledge
          [Struct "ggrandparent" [Atom "dusan", Var "X"]]
          [("X", Atom "nenad")]
    , testCase "family ?- ggrandparent2(dusan, X)." $
        succeedsWith
          familyKnowledge
          [Struct "ggrandparent2" [Atom "mitar", Var "X"]]
          [("X", Atom "nenad")]
    , testCase " ?- eq(int)." $
        succeedsWith
          eqTypeClassKnowledge
          [eq (Atom "int")]
          mempty
    , testCase "cyclic ?- eq(list(list(int)))." $
        succeedsWith
          cycleKnowledge
          [eq (Struct "list" [Struct "list" [Atom "int"]])]
          mempty
    , testCase "cyclic ?- eq(list(list(list(int))))." $
        succeedsWith
          cycleKnowledge
          [eq (Struct "list" [Struct "list" [Struct "list" [Atom "int"]]])]
          mempty
    ]

type TestTerm = Term String String
type TestClause = Clause String String

-- | The Greeks.
human :: TestTerm -> TestTerm
human x = Struct "human" [x]

animal :: TestTerm -> TestTerm
animal x = Struct "animal" [x]

platoIsHuman :: TestClause
platoIsHuman = human (Atom "plato") @<= []

socratesIsHuman :: TestClause
socratesIsHuman = human (Atom "socrates") @<= []

greekKnowledge :: [TestClause]
greekKnowledge =
  [ animal (Var "X") @<= [human (Var "X")]
  , socratesIsHuman
  , platoIsHuman
  ]

-- | The fam.
parent :: TestTerm -> TestTerm -> TestTerm
parent par child = Struct "parent" [par, child]

ancestor :: TestTerm -> TestTerm -> TestTerm
ancestor a d = Struct "ancestor" [a, d]

grandparent :: TestTerm -> TestTerm -> TestTerm
grandparent gpar gchild = Struct "grandparent" [gpar, gchild]

ancestorIsParent :: TestClause
ancestorIsParent =
  ancestor (Var "Anc") (Var "Dec")
    @<= [ parent (Var "Anc") (Var "Dec")
        ]

ancestorTransitive :: TestClause
ancestorTransitive =
  ancestor (Var "Anc") (Var "X")
    @<= [ parent (Var "Anc") (Var "X")
        , ancestor (Var "X") (Var "Dec")
        ]
familyKnowledge :: [TestClause]
familyKnowledge =
  [ parent (Atom "slavka") (Atom "nenad") @<= []
  , parent (Atom "zoran") (Atom "nenad") @<= []
  , parent (Atom "vlado") (Atom "zoran") @<= []
  , parent (Atom "ljeposava") (Atom "zoran") @<= []
  , parent (Atom "zdravka") (Atom "slavka") @<= []
  , parent (Atom "slavko") (Atom "slavka") @<= []
  , parent (Atom "mitar") (Atom "ljeposava") @<= []
  , parent (Atom "dusan") (Atom "vlado") @<= []
  , ancestorIsParent
  , ancestorTransitive
  , grandparent (Var "Gp") (Var "Gc")
      @<= [ parent (Var "Gp") (Var "P")
          , parent (Var "P") (Var "Gc")
          ]
  , Struct "ggrandparent" [Var "Ggp", Var "Ggc"]
      @<= [ parent (Var "Ggp") (Var "Gp")
          , parent (Var "Gp") (Var "P")
          , parent (Var "P") (Var "Ggc")
          ]
  , Struct "ggrandparent2" [Var "Ggp", Var "Ggc"]
      @<= [ Struct "grandparent" [Var "Ggp", Var "P"]
          , parent (Var "P") (Var "Ggc")
          ]
  ]

-- | Type Classes.
eq :: TestTerm -> TestTerm
eq ty = Struct "eq" [ty]

eqTypeClassKnowledge :: [TestClause]
eqTypeClassKnowledge =
  [ eq (Atom "int") @<= []
  , eq (Atom "bytes") @<= []
  , eq (Struct "maybe" [Var "A"]) @<= [eq (Var "A")]
  , eq (Struct "either" [Var "A", Var "B"]) @<= [eq (Var "A"), eq (Var "B")]
  , eq (Struct "var" [Var "X"]) @<= []
  ]

cycleKnowledge :: [TestClause]
cycleKnowledge =
  [ eq (Struct "list" [Var "X"]) @<= [eq (Var "X")]
  , eq (Struct "foo" [Var "X"]) @<= [eq (Struct "bar" [Var "X"])]
  , eq (Struct "bar" [Var "X"]) @<= [eq (Struct "baz" [Var "X"])]
  , eq (Struct "baz" [Var "X"]) @<= [eq (Struct "foo" [Var "X"])]
  , eq (Struct "beep" [Var "X"]) @<= [eq (Struct "beep" [Var "X"])]
  , eq (Atom "int") @<= []
  ]

-- | Testing actions.
succeedsWith :: [TestClause] -> [TestTerm] -> [(VarName, TestTerm)] -> Assertion
succeedsWith clauses goals wanted =
  let (errOrRes, logs) = solve clauses goals
   in case errOrRes of
        Left err -> do
          printLogs logs
          assertFailure $ show err
        Right got ->
          if got == Map.fromList wanted
            then return ()
            else do
              printLogs logs
              assertEqual "Solutions should match" (Map.fromList wanted) got

failsWith :: [TestClause] -> [TestTerm] -> MiniLogError String String -> Assertion
failsWith clauses goals wanted =
  let (errOrRes, logs) = solve clauses goals
   in case errOrRes of
        Left err ->
          if err == wanted
            then return ()
            else do
              printLogs logs
              assertEqual "" wanted err
        Right sols -> do
          printLogs logs
          assertFailure $ show ("Wanted an error but got" :: String, sols)

printLogs :: (Traversable t, Show a) => t a -> Assertion
printLogs logs = do
  putStrLn ""
  void $ print `traverse` logs
