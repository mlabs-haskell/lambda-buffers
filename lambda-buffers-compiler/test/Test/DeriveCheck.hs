{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.DeriveCheck (test) where

import Control.Lens (Prism, _Left, _Right)
import Control.Lens.Extras (is)
import Data.Generics.Labels ()
import Data.Generics.Sum.Constructors (AsConstructor (_Ctor))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck (runDeriveCheck)
import LambdaBuffers.Compiler.TypeClassCheck.Errors (
  BasicConditionViolation (OverlapDetected, TyConInContext),
  Instance,
  TypeClassError (BadInstance),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (
  Exp (AppE, DecE, LitE, NilE, RefE),
  ExpressionLike (nil, (*:), (*=)),
  Literal (ModuleName, Name, Opaque, TyVar),
  Pat (AppP, LitP, NilP, RefP),
  toProdE,
  toSumE,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Utils (
  ModuleBuilder (ModuleBuilder, mbClasses, mbInstances, mbScope, mbTyDefs),
  Tagged (Tag),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Validate (_X)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tagDef :: forall a. a -> Tagged a
tagDef = Tag PC.defSourceInfo

tagDefSet :: forall a. S.Set a -> S.Set (Tagged a)
tagDefSet = S.mapMonotonic tagDef

assertPrism :: forall s t a b. String -> Prism s t a b -> s -> Assertion
assertPrism str p s = assertBool str $ is p s

assertCtor :: forall l s t a b. (AsConstructor l s t a b) => String -> Either s () -> Assertion
assertCtor str = assertPrism str (_Left . _Ctor @l @s @t @a @b)

tyConInContext :: String -> Either TypeClassError () -> Assertion
tyConInContext msg e = assertBool msg $ case e of
  Left (BadInstance (TyConInContext _ _) _) -> True
  _ -> False

overlapDetected :: String -> Either TypeClassError () -> Assertion
overlapDetected msg e = assertBool msg $ case e of
  Left (BadInstance (OverlapDetected _) _) -> True
  _ -> False

test :: TestTree
test =
  testGroup
    "Derive Tests"
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    , test11
    ]
  where
    test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11 :: TestTree

    test1 =
      testCase "Basic simple module" $
        assertPrism "FAIL!" _Right (runTest moduleA'1)

    test2 =
      testCase "Module w/ imports" $
        assertPrism "FAIL!" _Right (runTest moduleB'1)

    test3 =
      testCase "Imports w/ tyvars" $
        assertPrism "FAIL!" _Right (runTest moduleB'2)

    test4 =
      testCase "Circular constraints" $
        tyConInContext
          "FAIL!"
          (runTest moduleB'3)

    test5 =
      testCase "Overlapping instances" $
        overlapDetected
          "FAIL!"
          (runTest moduleB'4)

    test6 =
      testCase "Missing Int instance" $
        assertCtor @"FailedToSolveConstraints"
          "FAIL!"
          (runTest moduleB'5)

    test7 =
      testCase "Constr in instance constraint" $
        tyConInContext
          "FAIL!"
          (runTest moduleB'6)

    test8 =
      testCase "Multiple TyVar data type" $
        assertPrism
          "FAIL!"
          _Right
          (runTest moduleC'1)

    test9 =
      testCase "Phantom tyvar arg" $
        assertPrism
          "FAIL"
          _Right
          (runTest moduleD'1)

    test10 =
      testCase "Phantom (concrete) type arg" $
        assertPrism
          "FAIL!"
          _Right
          (runTest moduleD'2)

    test11 =
      testCase "Missing type in phantom" $
        assertCtor @"FailedToSolveConstraints"
          "FAIL!"
          (runTest moduleD'3)

-- We should be able to avoid tricky superclass examples here
-- b/c we have them in the test suite for `solve`

pattern (:@) :: Pat -> Pat -> Pat
pattern (:@) p1 p2 = AppP p1 p2

pattern (:@@) :: Exp -> Exp -> Exp
pattern (:@@) p1 p2 = AppE p1 p2

pattern LocalRefP :: Text -> Pat
pattern LocalRefP nm = RefP NilP (LitP (Name nm))

pattern ForeignRefP :: [Text] -> Text -> Pat
pattern ForeignRefP mn nm = RefP (LitP (ModuleName mn)) (LitP (Name nm))

pattern ForeignRefE :: [Text] -> Text -> Exp
pattern ForeignRefE mn nm = RefE (LitE (ModuleName mn)) (LitE (Name nm))

pattern OpaqueDecNoVars :: Text -> Exp
pattern OpaqueDecNoVars nm = DecE (LitE (Name nm)) NilE (LitE Opaque)

-- No supers
_c :: Class
_c = Class (FQClassName "C" []) []

mkSum :: [(Text, Exp)] -> Exp
mkSum = toSumE . map go
  where
    go (n, t) = LitE (Name n) *= t

(.=) :: a -> b -> (a, b)
a .= b = (a, b)

infixr 0 .=

tyVarE :: Text -> Exp
tyVarE t = LitE (TyVar t)

tyVarP :: Text -> Pat
tyVarP t = LitP (TyVar t)

runTest :: ModuleBuilder -> Either TypeClassError ()
runTest = runDeriveCheck (moduleName1 "")
  where
    moduleName1 :: Text -> PC.ModuleName
    moduleName1 nm = PC.ModuleName [PC.ModuleNamePart nm si] si
    si = PC.SourceInfo "" pos pos
    pos = PC.SourcePosition 0 0

{- Test 1: Basic test. Should pass
module A where

opaque Int
opaque Bool
opaque String

data Maybe a = Nothing | Just a

class C a

instance C Int
instance C Bool
instance C String
instance C a => C (Maybe a)
-}
moduleA'1 :: ModuleBuilder
moduleA'1 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsA
    , mbInstances = tagDefSet instancesA
    , mbClasses = classesA
    , mbScope = S.empty
    }
  where
    tyDefsA :: M.Map Text Exp
    tyDefsA =
      M.fromList
        [ "Int" .= OpaqueDecNoVars "Int"
        , "String" .= OpaqueDecNoVars "String"
        , "Bool" .= OpaqueDecNoVars "Bool"
        , "Maybe" .= DecE (LitE (Name "Maybe")) (LitE (TyVar "a") *: NilE) $
            mkSum
              [ "Nothing" .= toProdE []
              , "Just" .= toProdE [LitE (TyVar "a")]
              ]
        ]

    instancesA :: S.Set Instance
    instancesA =
      S.fromList
        [ C _c (LocalRefP "Int") :<= []
        , C _c (LocalRefP "Bool") :<= []
        , C _c (LocalRefP "String") :<= []
        , C _c (LocalRefP "Maybe" :@ _X) :<= [C _c _X]
        ]

    classesA :: S.Set Class
    classesA = S.singleton _c

{- Test 2: With imports (have to do them by hand). Should pass

module B where

import A

data Foo = Foo (Maybe Int)

instance C Foo
-}

moduleB'1 :: ModuleBuilder
moduleB'1 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) NilE $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        , C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo") :<= []]

{- Test 3: With imports / tyvar

module A where (...same as above...)

---------------------

module B where

import A

data Foo a = Foo (Maybe a)

instance C a => C (Foo a)

-}

moduleB'2 :: ModuleBuilder
moduleB'2 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) (LitE (TyVar "a") *: nil) $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ LitE (TyVar "a")]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo" :@ _X) :<= [C _c _X]]

{- Test 4. Circular instant constraints, should fail condition 1

module B where

import A

data Foo a = Foo (Maybe a)

instance C (Foo a) => C (Foo a)
-}
moduleB'3 :: ModuleBuilder
moduleB'3 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = S.empty
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) (LitE (TyVar "a") *: nil) $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ LitE (TyVar "a")]
              ]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo" :@ _X) :<= [C _c (LocalRefP "Foo" :@ _X)]]

{- Test 5. Overlapping instances. Should fail w/ Overlap error

-- imports are as if this were A
module A where

(... same as above ...)
instance C (Maybe Int)

-----------------------------

module B where

data Foo = Foo (Maybe Int)

instance C Foo

-}
moduleB'4 :: ModuleBuilder
moduleB'4 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) NilE $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        , C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        , C _c (ForeignRefP ["A"] "Maybe" :@ ForeignRefP ["A"] "Int") :<= []
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo") :<= []]

{- Test 6. Should fail w/ missing Int instance

same as moduleB'2 w/ the imported (C Int) instance removed

-}
moduleB'5 :: ModuleBuilder
moduleB'5 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) NilE $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo") :<= []]

{- Test 7. Should fail w/ constructor in constraint context error.

same as moduleB'2 except the instance dec is replaced by:

instance C (Maybe Int) => C Foo

-}
moduleB'6 :: ModuleBuilder
moduleB'6 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsB
    , mbInstances = tagDefSet instancesB
    , mbClasses = S.empty
    , mbScope = S.empty
    }
  where
    tyDefsB :: M.Map Text Exp
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) NilE $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ ForeignRefE ["A"] "Int"]
              ]
        ]

    instancesB =
      S.fromList
        [C _c (LocalRefP "Foo") :<= [C _c (ForeignRefP ["A"] "Maybe" :@ ForeignRefP ["A"] "Int")]]

{- Test 8. Multiple TyVars. Should pass

module C where

import A

data Bar a b = MkBar a b Int

instance C (Bar a b)

-}
moduleC'1 :: ModuleBuilder
moduleC'1 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsC
    , mbInstances = tagDefSet instancesC
    , mbClasses = S.empty
    , mbScope = scopeC
    }
  where
    tyDefsC :: M.Map Text Exp
    tyDefsC =
      M.fromList
        [ "Bar" .= DecE (LitE (Name "Bar")) (tyVarE "a" *: tyVarE "b" *: nil) $
            mkSum
              [ "MkBar" .= toProdE [tyVarE "a", tyVarE "b", ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeC =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        ]

    instancesC = S.fromList [C _c (LocalRefP "Bar" :@ tyVarP "a" :@ tyVarP "b") :<= []]

{- Test 9. Phantom tyvar. Should pass

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a b)

-}
moduleD'1 :: ModuleBuilder
moduleD'1 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsD
    , mbInstances = tagDefSet instancesD
    , mbClasses = S.empty
    , mbScope = scopeD
    }
  where
    tyDefsD :: M.Map Text Exp
    tyDefsD =
      M.fromList
        [ "Bar" .= DecE (LitE (Name "Bar")) (tyVarE "a" *: tyVarE "b" *: nil) $
            mkSum
              [ "MkBar" .= toProdE [tyVarE "a", ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeD =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        ]

    instancesD = S.fromList [C _c (LocalRefP "Bar" :@ tyVarP "a" :@ tyVarP "b") :<= []]

{- Test 10. Phantom concrete type. Should pass (b/c instance isn't needed for phantom type)

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a Bool)

-}
moduleD'2 :: ModuleBuilder
moduleD'2 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsD
    , mbInstances = tagDefSet instancesD
    , mbClasses = S.empty
    , mbScope = scopeD
    }
  where
    tyDefsD :: M.Map Text Exp
    tyDefsD =
      M.fromList
        [ "Bar" .= DecE (LitE (Name "Bar")) (tyVarE "a" *: tyVarE "b" *: nil) $
            mkSum
              [ "MkBar" .= toProdE [tyVarE "a", ForeignRefE ["A"] "Int"]
              ]
        ]

    scopeD =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        ]

    instancesD =
      S.fromList
        [ C _c (LocalRefP "Bar" :@ tyVarP "a" :@ ForeignRefP ["A"] "Bool") :<= []
        ]

{- Test 11. Modification of test 9 where non-phantom instance is missing. Should fail w/ missing Int instance

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a Bool)

-}
moduleD'3 :: ModuleBuilder
moduleD'3 =
  ModuleBuilder
    { mbTyDefs = tagDef <$> tyDefsD
    , mbInstances = tagDefSet instancesD
    , mbClasses = S.empty
    , mbScope = S.empty
    }
  where
    tyDefsD :: M.Map Text Exp
    tyDefsD =
      M.fromList
        [ "Bar" .= DecE (LitE (Name "Bar")) (tyVarE "a" *: tyVarE "b" *: nil) $
            mkSum
              [ "MkBar" .= toProdE [tyVarE "a", ForeignRefE ["A"] "Int"]
              ]
        ]

    instancesD =
      S.fromList
        [ C _c (LocalRefP "Bar" :@ tyVarP "a" :@ ForeignRefP ["A"] "Bool") :<= []
        ]
