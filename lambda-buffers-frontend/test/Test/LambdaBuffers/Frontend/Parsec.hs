module Test.LambdaBuffers.Frontend.Parsec (tests) where

import Test.Tasty (TestTree, testGroup)

import LambdaBuffers.Frontend.Parsec (parseProduct, parseRecord, parseSum, parseTyInner, parseTyTopLevel)
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
    ]

testInnerTypeExpression :: TestTree
testInnerTypeExpression =
  testGroup
    "inner type expression"
    [ testGroup
        "parses"
        [ parses "a" parseTyInner
        , parses " a" parseTyInner
        , parses "(a)" parseTyInner
        , parses "( a )" parseTyInner
        , parses "(  (a   ) )" parseTyInner
        , parses "Int" parseTyInner
        , parses " Int" parseTyInner
        , parses "(Int)" parseTyInner
        , parses "(Maybe a)" parseTyInner
        , parses "(Maybe Int)" parseTyInner
        , parses "(Maybe Int )" parseTyInner
        , parses "((Maybe) a)" parseTyInner
        , parses "((Maybe) (a))" parseTyInner
        , parses "(Maybe (a ) ( c) )" parseTyInner
        , parses "(Maybe Int String a b)" parseTyInner
        , parses "( Maybe ( Maybe ( Maybe (Maybe a ))))" parseTyInner
        , parses "(Maybe (A a) b (c) (d) )" parseTyInner
        , parses "(Maybe(Int))" parseTyInner
        , parses "(Maybe((Int)))" parseTyInner
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
        [ parses "a" parseTyTopLevel
        , parses " a" parseTyTopLevel
        , parses "(a)" parseTyTopLevel
        , parses "( a )" parseTyTopLevel
        , parses "(  (a   ) )" parseTyTopLevel
        , parses "Int" parseTyTopLevel
        , parses " Int" parseTyTopLevel
        , parses "(Int)" parseTyTopLevel
        , parses "(Maybe a)" parseTyTopLevel
        , parses "(Maybe Int)" parseTyTopLevel
        , parses "(Maybe Int )" parseTyTopLevel
        , parses "((Maybe) a)" parseTyTopLevel
        , parses "((Maybe) (a))" parseTyTopLevel
        , parses "(Maybe (a ) ( c) )" parseTyTopLevel
        , parses "(Maybe Int String a b)" parseTyTopLevel
        , parses "( Maybe ( Maybe ( Maybe (Maybe a ))))" parseTyTopLevel
        , parses "(Maybe (A a) b (c) (d) )" parseTyTopLevel
        , parses "Maybe a" parseTyTopLevel
        , parses "Maybe Int" parseTyTopLevel
        , parses "Maybe a b c" parseTyTopLevel
        , parses "Maybe a Int b String" parseTyTopLevel
        , parses "Maybe(Int)" parseTyTopLevel
        , parses "Maybe((Int))" parseTyTopLevel
        , parses "Maybe\n Int" parseTyTopLevel
        , parses "Maybe \n Int" parseTyTopLevel
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
        , parses "{x : a}" parseRecord
        , parses "{x : Int}" parseRecord
        , parses "{x : Maybe a}" parseRecord
        , parses "{x : Either a b}" parseRecord
        , parses "{x : (a)}" parseRecord
        , parses "{x : (Int)}" parseRecord
        , parses "{x : (Maybe a)}" parseRecord
        , parses "{x : (Either a b)}" parseRecord
        , parses "{x : a, y : Int, z : Maybe a}" parseRecord
        , parses "{  x : a,y : Int , z : Maybe a }" parseRecord
        , parses "{\n x : a,\n y : Int ,\n z : Maybe a\n }" parseRecord
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

parses :: String -> Parsec String () a -> TestTree
parses input parser = testCase input $ case runParser (parser <* eof) () "test" input of
  Left err -> assertFailure (show err)
  Right _ -> return ()

fails :: Show a => String -> Parsec String () a -> TestTree
fails input parser = testCase input $ case runParser (parser <* eof) () "test" input of
  Left _ -> return ()
  Right res -> assertFailure (show res)
