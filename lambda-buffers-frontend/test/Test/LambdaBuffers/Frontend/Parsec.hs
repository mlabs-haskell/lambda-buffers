module Test.LambdaBuffers.Frontend.Parsec (tests) where

import Test.Tasty (TestTree, testGroup)

import Control.Monad (void)
import Data.Set qualified as Set
import LambdaBuffers.Frontend.Parsec (junk, parseClassDef, parseClassSups, parseConstraint, parseDerive, parseInstanceBody, parseInstanceClause, parseProduct, parseRecord, parseSum, parseTyInner, parseTyTopLevel)
import LambdaBuffers.Frontend.Syntax (ClassConstraint, Constraint, SourceInfo)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Parsec (Parsec, eof, runParser)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Frontend.Parsec"
    [ testInnerTypeExpression
    , testTopLevelTypeExpression
    , testRecordExpression
    , testProductExpression
    , testSumExpression
    , testConstraintExpression
    , testInstanceBodyExpression
    , testInstanceClause
    , testDerive
    , testClassSups
    , testClassDef
    ]

testInnerTypeExpression :: TestTree
testInnerTypeExpression =
  testGroup
    "inner type expression"
    [ testGroup
        "parses"
        [ parsesEq ["a", " a", "a ", " a ", "(a)", "( a )", "(  (a   ) )"] parseTyInner
        , parsesEq ["Int", " Int", "Int ", "(Int)", "( Int)", "(Int )", " (Int)", "(Int) ", "((Int))"] parseTyInner
        , -- TODO: this test case is screwed.. there's problems with the data
          -- representation for why this won't pass e.g. @A a a@ is @A@ applied
          -- to the list @[a,a]@; so breaking this down to the left associative
          -- chain of applications really is broken.
          -- , parsesEq
          --     [ "(A.B.A a a a)"
          --     , "((A.B.A a) a a)"
          --     ]
          --     parseTyInner
          parsesEq
            [ "(Maybe a)"
            , " (Maybe a)"
            , "(Maybe a) "
            , "( Maybe a)"
            , "(Maybe a )"
            , "(Maybe (a))"
            , "((Maybe) a)"
            , "((Maybe a))"
            , "((Maybe) (a))"
            , "(Maybe(a))"
            , "(Maybe((a)))"
            , "(Maybe\n ((a)))"
            ]
            parseTyInner
        , parsesEq
            [ "(Maybe Int)"
            , " (Maybe Int)"
            , "(Maybe Int) "
            , "( Maybe Int)"
            , "(Maybe Int )"
            , "(Maybe (Int))"
            , "((Maybe) Int)"
            , "((Maybe Int))"
            , "((Maybe) (Int))"
            , "(Maybe(Int))"
            , "(Maybe((Int)))"
            , "(Maybe\n ((Int)))"
            ]
            parseTyInner
        , parsesEq
            [ "(Prelude.Maybe Prelude.Numeric.Int)"
            , " (Prelude.Maybe Prelude.Numeric.Int)"
            , "(Prelude.Maybe Prelude.Numeric.Int) "
            , "( Prelude.Maybe Prelude.Numeric.Int)"
            , "(Prelude.Maybe Prelude.Numeric.Int )"
            , "(Prelude.Maybe (Prelude.Numeric.Int))"
            , "((Prelude.Maybe) Prelude.Numeric.Int)"
            , "((Prelude.Maybe Prelude.Numeric.Int))"
            , "((Prelude.Maybe) (Prelude.Numeric.Int))"
            , "(Prelude.Maybe(Prelude.Numeric.Int))"
            , "(Prelude.Maybe((Prelude.Numeric.Int)))"
            , "(Prelude.Maybe\n ((Prelude.Numeric.Int)))"
            ]
            parseTyInner
        , parsesEq ["(Either a b)", "(Either (a) (b))", "(Either(a)(b))"] parseTyInner
        , parses "(Maybe Int String a b)" parseTyInner
        , parses "( Maybe ( Maybe ( Maybe (Maybe a ))))" parseTyInner
        , parses "((((Maybe) a) b) c)" parseTyInner
        , parses "(Maybe (A a) b (c) (d) )" parseTyInner
        ]
    , testGroup
        "fails"
        [ fails "((a)" parseTyInner
        , fails "( a ))" parseTyInner
        , fails "(  (a   ) ))" parseTyInner
        , fails "(Int))" parseTyInner
        , fails "Maybe a" parseTyInner
        , fails "Maybe Int" parseTyInner
        ]
    ]

testTopLevelTypeExpression :: TestTree
testTopLevelTypeExpression =
  testGroup
    "tope level type expression"
    [ testGroup
        "parses"
        [ parsesEq ["a", " a", "a ", " a ", "(a)", "( a )", "(  (a   ) )"] parseTyTopLevel
        , parsesEq ["Int", " Int", "Int ", "(Int)", "( Int)", "(Int )", " (Int)", "(Int) ", "((Int))"] parseTyTopLevel
        , parsesEq
            [ "Maybe a"
            , " Maybe a"
            , "Maybe a "
            , " Maybe a "
            , "(Maybe a)"
            , " (Maybe a)"
            , "(Maybe a) "
            , "( Maybe a)"
            , "(Maybe a )"
            , "(Maybe (a))"
            , "((Maybe) a)"
            , "((Maybe a))"
            , "((Maybe) (a))"
            , "(Maybe(a))"
            , "(Maybe((a)))"
            , "(Maybe\n ((a)))"
            ]
            parseTyTopLevel
        , parsesEq
            [ "Maybe Int"
            , " Maybe Int"
            , "Maybe Int "
            , " Maybe Int "
            , "(Maybe Int)"
            , " (Maybe Int)"
            , "(Maybe Int) "
            , "( Maybe Int)"
            , "(Maybe Int )"
            , "(Maybe (Int))"
            , "((Maybe) Int)"
            , "((Maybe Int))"
            , "((Maybe) (Int))"
            , "(Maybe(Int))"
            , "(Maybe((Int)))"
            , "(Maybe\n ((Int)))"
            ]
            parseTyTopLevel
        , parsesEq
            [ "Prelude.Maybe Prelude.Numeric.Int"
            , " Prelude.Maybe Prelude.Numeric.Int"
            , "Prelude.Maybe Prelude.Numeric.Int "
            , " Prelude.Maybe Prelude.Numeric.Int "
            , "(Prelude.Maybe Prelude.Numeric.Int)"
            , " (Prelude.Maybe Prelude.Numeric.Int)"
            , "(Prelude.Maybe Prelude.Numeric.Int) "
            , "( Prelude.Maybe Prelude.Numeric.Int)"
            , "(Prelude.Maybe Prelude.Numeric.Int )"
            , "(Prelude.Maybe (Prelude.Numeric.Int))"
            , "((Prelude.Maybe) Prelude.Numeric.Int)"
            , "((Prelude.Maybe Prelude.Numeric.Int))"
            , "((Prelude.Maybe) (Prelude.Numeric.Int))"
            , "(Prelude.Maybe(Prelude.Numeric.Int))"
            , "(Prelude.Maybe((Prelude.Numeric.Int)))"
            , "(Prelude.Maybe\n ((Prelude.Numeric.Int)))"
            ]
            parseTyTopLevel
        , parsesEq
            [ "Either a b"
            , "Either (a) (b)"
            , "Either(a)(b)"
            , "(Either a b)"
            , "(Either (a) (b))"
            , "(Either(a)(b))"
            ]
            parseTyTopLevel
        , parsesEq ["Maybe Int String a b", "(Maybe Int String a b)"] parseTyTopLevel
        , parses "( Maybe ( Maybe ( Maybe (Maybe a ))))" parseTyTopLevel
        , parses "((((Maybe) a) b) c)" parseTyTopLevel
        , parses "(Maybe (A a) b (c) (d) )" parseTyTopLevel
        , parses "(Maybe (a ) ( c) )" parseTyTopLevel
        , parses "(Maybe Int String a b)" parseTyTopLevel
        , parses "( Maybe ( Maybe ( Maybe (Maybe a ))))" parseTyTopLevel
        , parses "(Maybe (A a) b (c) (d) )" parseTyTopLevel
        , parses "Maybe a Int b String" parseTyTopLevel
        , parses "Maybe\na" parseTyTopLevel
        , parses "Maybe \na" parseTyTopLevel
        , parses "Maybe a\n" parseTyTopLevel
        ]
    , testGroup
        "fails"
        [ fails "((a)" parseTyTopLevel
        , fails "( a ))" parseTyTopLevel
        , fails "(  (a   ) ))" parseTyTopLevel
        , fails "(Int))" parseTyTopLevel
        ]
    ]

testRecordExpression :: TestTree
testRecordExpression =
  testGroup
    "record type expression"
    [ testGroup
        "parses"
        [ parses "{}" parseRecord
        , parsesEq ["{x : a}", "{ x : a}", "{x : a }", "{ x : a }", "{x : (a)}"] parseRecord
        , parsesEq ["{x : Int}", "{ x : Int}", "{x : Int }", "{ x : Int }", "{x : (Int)}"] parseRecord
        , parsesEq ["{x : Maybe a}", "{ x : Maybe a}", "{x : Maybe a }", "{ x : Maybe a }", "{x : (Maybe a)}"] parseRecord
        , parsesEq ["{x : Either a b}", "{ x : Either a b}", "{x : Either a b }", "{ x : Either a b }", "{x : (Either a b)}"] parseRecord
        , parsesEq ["{x : a, y : Int, z : Maybe a}", "{  x : a,y : Int , z : Maybe a }", "{\n x : a,\n y : Int ,\n z : Maybe a\n }"] parseRecord
        , parsesEq ["{x : a, y : Prelude.Numeric.Int, z : Prelude.Maybe a}", "{  x : a,y : Prelude.Numeric.Int , z : Prelude.Maybe a }", "{\n x : a,\n y : Prelude.Numeric.Int ,\n z : Prelude.Maybe a\n }"] parseRecord
        , parses "{x:y}" parseRecord
        , parses "{ x:y }" parseRecord
        , parses "{ x: y}" parseRecord
        , parses "{ x :y}" parseRecord
        , parses "{x :y}" parseRecord
        , parses "{x: y}" parseRecord
        , parses "{\nx : a}" parseRecord
        , parses "{x\n: a}" parseRecord
        , parses "{x :\na}" parseRecord
        , parses "{x : a\n}" parseRecord
        , parses " {}" parseRecord
        ]
    , testGroup
        "fails"
        [ fails "{x}" parseRecord
        , fails "{ x }" parseRecord
        , fails "{ x: }" parseRecord
        ]
    ]

testProductExpression :: TestTree
testProductExpression =
  testGroup
    "product type expression"
    [ testGroup
        "parses"
        [ parses "" parseProduct
        , parses "a" parseProduct
        , parses "Int" parseProduct
        , parses "Maybe a" parseProduct
        , parses "Either a b" parseProduct
        , parses "(a)" parseProduct
        , parses "(Int)" parseProduct
        , parses "(Maybe a)" parseProduct
        , parses "(Either a b)" parseProduct
        , parses "a Int (Maybe a)" parseProduct
        , parses "   a y  Int  z  (Maybe a) " parseProduct
        , parses "Maybe\n Int" parseProduct
        , parses "\n" parseProduct
        , parses "\nMaybe Int" parseProduct
        , parses "Maybe \nInt" parseProduct
        , parses "Maybe Int\n" parseProduct
        ]
    , testGroup
        "fails"
        [ fails "()" parseProduct
        , fails "(    ) -- dog" parseProduct
        ]
    ]

testSumExpression :: TestTree
testSumExpression =
  testGroup
    "sum type expression"
    [ testGroup
        "parses"
        [ parses "" parseSum
        , parses "A | B" parseSum
        , parses "A|B" parseSum
        , parses "A|B|C" parseSum
        , parses "A  |   B|  C" parseSum
        , parses "A(a)| B(b)" parseSum
        , parses "A a | B b" parseSum
        , parses "A a b | B b a" parseSum
        , parses "A a b | B b a | C c d" parseSum
        , parses "A ((a) b) | B (b a) | C (c) (d)" parseSum
        , parses "A Int (Maybe Int String) | B (Prelude.Maybe a) | C Prelude.Numeric.Int Prelude.Numeric.String" parseSum
        , parses "\n" parseSum
        ]
    , testGroup
        "fails"
        [ fails "A |" parseSum
        , fails "A ()| B" parseSum
        , fails "A | B ()" parseSum
        , fails "A (B | C)" parseSum
        ]
    ]

testConstraintExpression :: TestTree
testConstraintExpression =
  testGroup
    "constraint expression"
    [ testGroup
        "parses"
        [ parsesEq ["Eq a", "Eq a", "Eq  a", "Eq (a)", "Eq ((a))"] parseConstraint
        , parsesEq ["Eq Int", "Eq Int", "Eq  Int", "Eq (Int)", "Eq ((Int))"] parseConstraint
        , parsesEq ["Eq Prelude.Numeric.Int", "Eq Prelude.Numeric.Int", "Eq  Prelude.Numeric.Int", "Eq (Prelude.Numeric.Int)", "Eq ((Prelude.Numeric.Int))"] parseConstraint
        , parsesEq ["Eq (Maybe a)", "Eq ((Maybe a))"] parseConstraint
        , parsesEq ["Eq (Prelude.Maybe a)", "Eq ((Prelude.Maybe a))"] parseConstraint
        , parsesEq ["Prelude.Eq (Maybe a)", "Prelude.Eq ((Maybe a))"] parseConstraint
        , parses "MPTC Int (Maybe a) a (Either a b)" parseConstraint
        , parses "Eq (Either (Maybe a) (List Int))" parseConstraint
        , parses "Trivial" parseConstraint
        ]
    , testGroup
        "fails"
        [ fails "\n" parseConstraint
        , fails "" parseConstraint
        , fails "eq a" parseConstraint
        , fails "(Eq a)" parseConstraint
        , fails "a" parseConstraint
        ]
    ]

newtype IB info = IB [Constraint info] deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

testInstanceBodyExpression :: TestTree
testInstanceBodyExpression =
  testGroup
    "instance body expression"
    [ testGroup
        "parses"
        [ parsesEq ["", "()"] parseIB -- TODO(bladyjoker): Figure out ().
        , parsesEq ["Eq a, Show a", "(Eq a, Show a)"] parseIB
        , parsesEq ["Eq a", "Eq  a", "Eq a ", " Eq a", "\n Eq a", "Eq\n a"] parseIB
        , parsesEq ["Eq a, Eq b", "Eq a , Eq b", "Eq a\n , Eq b", "Eq a\n , Eq b, ()"] parseIB
        , parses "Eq Int" parseIB
        , parses "MPTC Int (Maybe a) a (Either a b)" parseIB
        , parses "Eq (Either (Maybe a) (List Int))" parseIB
        , parses "Trivial" parseIB
        , parses "()" parseIB
        , parses "(Eq a, Show b, Json c, MPTC (Maybe a) (Either a Int) c)" parseIB
        , parsesEq
            [ "Eq a, Show b, Json c, MPTC (Maybe a) (Either a Int) c"
            , "Eq a, Show b, Json c, (MPTC (Maybe a) (Either a Int) c)"
            , "Eq a, Show b, (Json c, (MPTC (Maybe a) (Either a Int) c))"
            , "Eq a, (Show b, (Json c, (MPTC (Maybe a) (Either a Int) c)))"
            , "(Eq a, (Show b, (Json c, (MPTC (Maybe a) (Either a Int) c))))"
            , "((Eq a, (Show b, (Json c, (MPTC (Maybe a) (Either a Int) c)))))"
            , "(Eq a), Show b, Json c, MPTC (Maybe a) (Either a Int) c"
            , "(Eq a, Show b), (Json c, MPTC (Maybe a) (Either a Int) c)"
            , "Eq a, (Show b, Json c), MPTC (Maybe a) (Either a Int) c"
            ]
            parseIB
        , parses "\n" parseIB
        ]
    , testGroup
        "fails"
        [ fails "eq a" parseIB
        , fails "a" parseIB
        , fails "Eq a," parseIB
        , fails "Eq a, " parseIB
        ]
    ]
  where
    parseIB :: Parsec String () (IB SourceInfo)
    parseIB = IB <$> parseInstanceBody

testInstanceClause :: TestTree
testInstanceClause =
  testGroup
    "instance clause"
    [ testGroup
        "parses"
        [ parsesEq ["instance Eq (Maybe a) :- Eq a", "instance  Eq  ( Maybe  a )  :-  Eq  a "] parseInstanceClause
        , parses "instance Eq (Maybe a) :- Eq a, Eq b, Eq (Maybe a b)" parseInstanceClause
        , parses "instance Eq a :- Eq a " parseInstanceClause
        , parses "instance Eq (Maybe a) :- ()" parseInstanceClause
        , parses "instance Eq (Either a b) :- Eq a, Eq b" parseInstanceClause
        , parses "instance Eq (Either Int String) :- Eq Int, Eq String" parseInstanceClause
        , parses "instance Prelude.Eq (Prelude.Either Prelude.Numeric.Int Prelude.String) :- Prelude.Eq Prelude.Numeric.Int, Prelude.Eq Prelude.String" parseInstanceClause
        , parsesEq ["instance Eq (Maybe a)", "instance Eq (Maybe a) :- ()"] parseInstanceClause
        ]
    , testGroup
        "fails"
        [ fails "\n" parseInstanceClause
        , fails "" parseInstanceClause
        , fails "instance (Eq (Maybe a)) :- Eq a" parseInstanceClause
        ]
    ]

testDerive :: TestTree
testDerive =
  testGroup
    "derive"
    [ testGroup
        "parses"
        [ parsesEq ["derive Eq (Maybe a)", "derive  Eq  ( Maybe  a )  "] parseDerive
        , parsesEq ["derive Eq a", "derive Eq (a)"] parseDerive
        , parsesEq ["derive Eq Int", "derive Eq (Int)"] parseDerive
        , parses "derive Eq (Either a b)" parseDerive
        , parses "derive Eq (Either Int String)" parseDerive
        ]
    , testGroup
        "fails"
        [ fails "\n" parseDerive
        , fails "" parseDerive
        , fails "derive (Eq (Maybe a))" parseDerive
        ]
    ]

newtype CS info = CS [ClassConstraint info] deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

testClassSups :: TestTree
testClassSups =
  testGroup
    "class supers expression"
    [ testGroup
        "parses"
        [ parses "" parseCS
        , parses "Eq a" parseCS
        , parses " Eq a" parseCS
        , parses " Eq a, Show a, Eq b" parseCS
        , parsesEq
            [ " Eq a, Show a, Eq b"
            , " Eq a, (Show a, Eq b)"
            , " (Eq a, Show a, Eq b)"
            , " (Eq a, Show a), Eq b"
            ]
            parseCS
        , -- FIX(bladyjoker): parses "Eq a " parseCS
          parsesEq
            [ "Eq a"
            , "Eq  a"
            , " Eq a"
            , " Eq   a"
            , "(Eq a)"
            , "(Eq  a)"
            , "( Eq  a)"
            , "(Eq  a)"
            ]
            parseCS
        , parses "\n" parseCS
        ]
    , testGroup
        "fails"
        [ fails "Eq Int" parseCS
        , fails "Eq Int, show Int" parseCS
        ]
    ]
  where
    parseCS :: Parsec String () (CS SourceInfo)
    parseCS = CS <$> parseClassSups

testClassDef :: TestTree
testClassDef =
  testGroup
    "class definition"
    [ testGroup
        "parses"
        [ parsesEq
            [ "class Eq a"
            , "class  Eq  a"
            ]
            parseClassDef
        , parsesEq
            [ "class (Eq a) <= Ord a"
            , "class (Eq  a) <= Ord a"
            , "class ( Eq  a) <= Ord a"
            ]
            parseClassDef
        , parsesEq
            [ "class (Eq a), Eq b <= Ord a"
            , "class (Eq  a, Eq b) <= Ord a"
            , "class Eq  a ,        Eq b <= Ord a"
            ]
            parseClassDef
        , parsesEq
            [ "class Trivial"
            , "class  Trivial"
            , "class   Trivial"
            ]
            parseClassDef
        , parsesEq
            [ "class (MPTC1 b a, MPTC2 c b a) <= MPTC a b c"
            , "class ((MPTC1 b a), (MPTC2 c b a)) <= MPTC a b c"
            , "class ((MPTC1 b a, MPTC2 c b a)) <= MPTC a b c"
            ]
            parseClassDef
        , parses " class Eq a" parseClassDef
        , parses "class Eq a " parseClassDef
        , parses "class () <= Eq a" parseClassDef
        ]
    , testGroup
        "fails"
        [ fails "\n" parseClassDef
        , fails "" parseClassDef
        , fails "class Eq a <=" parseClassDef
        , fails "class Eq a <= " parseClassDef
        , fails "class Eq a, Eq a <= Eq a<= Eq a" parseClassDef
        , fails "class (Eq a)" parseClassDef
        ]
    ]

parsesEq :: forall a info. (Functor a, Show (a ()), Ord (a ())) => [String] -> Parsec String () (a info) -> TestTree
parsesEq inputs parser =
  testCase (show inputs <> " should parse the same") $
    let ress = runParser (junk *> parser <* eof) () "test" <$> inputs
     in case foldr
          ( \res (errs, ps) -> case res of
              Left err -> (err : errs, ps)
              Right p -> (errs, void p `Set.insert` ps)
          )
          (mempty, Set.empty)
          ress of
          ([], ps) | length ps == 1 -> return ()
          (errs, ps) -> assertFailure $ show ("Wanted all to parse the same" :: String, errs, ps)

parses :: String -> Parsec String () a -> TestTree
parses input parser = testCase (show input) $ case runParser (junk *> parser <* eof) () "test" input of
  Left err -> assertFailure (show err)
  Right _ -> return ()

fails :: Show a => String -> Parsec String () a -> TestTree
fails input parser = testCase (show input) $ case runParser (junk *> parser <* eof) () "test" input of
  Left _ -> return ()
  Right res -> assertFailure (show res)
