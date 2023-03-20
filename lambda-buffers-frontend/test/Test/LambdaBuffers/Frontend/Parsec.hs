module Test.LambdaBuffers.Frontend.Parsec (tests) where

import Test.Tasty (TestTree, testGroup)

import Control.Monad (void)
import Data.Set qualified as Set
import LambdaBuffers.Frontend.Parsec (parseConstraint, parseDerive, parseInstanceBody, parseInstanceClause, parseProduct, parseRecord, parseSum, parseTyInner, parseTyTopLevel)
import LambdaBuffers.Frontend.Syntax (Constraint, SourceInfo)
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
    ]

testInnerTypeExpression :: TestTree
testInnerTypeExpression =
  testGroup
    "inner type expression"
    [ testGroup
        "parses"
        [ parsesEq ["a", " a", "a ", " a ", "(a)", "( a )", "(  (a   ) )"] parseTyInner
        , parsesEq ["Int", " Int", "Int ", "(Int)", "( Int)", "(Int )", " (Int)", "(Int) ", "((Int))"] parseTyInner
        , parsesEq
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
        ]
    , testGroup
        "fails"
        [ fails "((a)" parseTyTopLevel
        , fails "( a ))" parseTyTopLevel
        , fails "(  (a   ) ))" parseTyTopLevel
        , fails "(Int))" parseTyTopLevel
        , fails "Maybe\na" parseTyTopLevel
        , fails "Maybe \na" parseTyTopLevel
        , fails "Maybe a\n" parseTyTopLevel
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
        ]
    , testGroup
        "fails"
        [ fails " {}" parseRecord
        , fails "{x}" parseRecord
        , fails "{ x }" parseRecord
        , fails "{x:y}" parseRecord
        , fails "{ x:y }" parseRecord
        , fails "{ x: }" parseRecord
        , fails "{ x: y}" parseRecord
        , fails "{ x :y}" parseRecord
        , fails "{x :y}" parseRecord
        , fails "{x: y}" parseRecord
        , fails "{\nx : a}" parseRecord
        , fails "{x\n: a}" parseRecord
        , fails "{x :\na}" parseRecord
        , fails "{x : a\n}" parseRecord
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
        ]
    , testGroup
        "fails"
        [ fails "\n" parseProduct
        , fails "\nMaybe Int" parseProduct
        , fails "Maybe \nInt" parseProduct
        , fails "Maybe Int\n" parseProduct
        , fails "()" parseProduct
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
        ]
    , testGroup
        "fails"
        [ fails "\n" parseSum
        , fails "A |" parseSum
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
        , parsesEq ["Eq (Maybe a)", "Eq ((Maybe a))"] parseConstraint
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
        ]
    , testGroup
        "fails"
        [ fails "\n" parseIB
        , fails "eq a" parseIB
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

parsesEq :: forall a info. (Functor a, Show (a ()), Ord (a ())) => [String] -> Parsec String () (a info) -> TestTree
parsesEq inputs parser =
  testCase (show inputs <> " should parse the same") $
    let ress = runParser (parser <* eof) () "test" <$> inputs
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
parses input parser = testCase input $ case runParser (parser <* eof) () "test" input of
  Left err -> assertFailure (show err)
  Right _ -> return ()

fails :: Show a => String -> Parsec String () a -> TestTree
fails input parser = testCase input $ case runParser (parser <* eof) () "test" input of
  Left _ -> return ()
  Right res -> assertFailure (show res)
