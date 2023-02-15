{-# LANGUAGE PatternSynonyms #-}

module Test.DeriveCheck where

import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Utils
import LambdaBuffers.Compiler.TypeClass.Validate

-- We should be able to avoid tricky superclass examples here
-- b/c we have them in the test suite for `solve`

import Control.Monad (void)
import Data.Map qualified as M
import Data.Set qualified as S
import LambdaBuffers.Compiler.TypeClass.Rules
import LambdaBuffers.Compiler.TypeClassCheck (runDeriveCheck)
import Prettyprinter

pattern (:@) :: Pat -> Pat -> Pat
pattern (:@) p1 p2 = AppP p1 p2

pattern (:@@) :: Exp -> Exp -> Exp
pattern (:@@) p1 p2 = AppE p1 p2

pattern LocalRefP :: Text -> Pat
pattern LocalRefP nm = RefP NilP (LitP (Name nm))

pattern ForeignRefP :: [Text] -> Text -> Pat
pattern ForeignRefP mn nm = RefP (LitP (ModuleName mn)) (LitP (Name nm))

pattern LocalRefE :: Text -> Exp
pattern LocalRefE nm = RefE NilE (LitE (Name nm))

pattern ForeignRefE :: [Text] -> Text -> Exp
pattern ForeignRefE mn nm = RefE (LitE (ModuleName mn)) (LitE (Name nm))

pattern OpaqueDecNoVars :: Text -> Exp
pattern OpaqueDecNoVars nm = DecE (LitE (Name nm)) NilE (LitE Opaque)

moduleName1 :: Text -> PC.ModuleName
moduleName1 nm = PC.ModuleName [PC.ModuleNamePart nm si] si
  where
    si :: PC.SourceInfo
    si = PC.SourceInfo "" pos pos
    pos :: PC.SourcePosition
    pos = PC.SourcePosition 0 0

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

runTest nm mod = case runDeriveCheck (moduleName1 nm) mod of
  Left e -> print $ pretty e
  Right _ -> pure ()

runDeriveTest :: M.Map PC.ModuleName ModuleBuilder -> Either TypeClassError ()
runDeriveTest = void . M.traverseWithKey runDeriveCheck

{-
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
moduleA :: ModuleBuilder
moduleA =
  ModuleBuilder
    { mbTyDefs = tyDefsA
    , mbInstances = instancesA
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

-- happy paths

{- Test 2: With imports (have to do them by hand)

module A where (...same as above...)

------------------------------------

module B where

import A

data Foo = Foo (Maybe Int)

instance C Foo
-}

moduleB'1 :: ModuleBuilder
moduleB'1 =
  ModuleBuilder
    { mbTyDefs = tyDefsB
    , mbInstances = instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
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
        , C _c (ForeignRefP ["A"] "Bool") :<= []
        , C _c (ForeignRefP ["A"] "String") :<= []
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
    { mbTyDefs = tyDefsB
    , mbInstances = instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) (LitE (TyVar "a") *: nil) $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ LitE (TyVar "a")]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        , C _c (ForeignRefP ["A"] "Bool") :<= []
        , C _c (ForeignRefP ["A"] "String") :<= []
        , C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo" :@ _X) :<= [C _c _X]]

-- unhappy paths

-- circular instant constraints, should fail condition 1
moduleB'3 :: ModuleBuilder
moduleB'3 =
  ModuleBuilder
    { mbTyDefs = tyDefsB
    , mbInstances = instancesB
    , mbClasses = S.empty
    , mbScope = scopeB
    }
  where
    tyDefsB =
      M.fromList
        [ "Foo" .= DecE (LitE (Name "Foo")) (LitE (TyVar "a") *: nil) $
            mkSum
              [ "Foo" .= toProdE [ForeignRefE ["A"] "Maybe" :@@ LitE (TyVar "a")]
              ]
        ]

    scopeB =
      S.fromList
        [ C _c (ForeignRefP ["A"] "Int") :<= []
        , C _c (ForeignRefP ["A"] "Bool") :<= []
        , C _c (ForeignRefP ["A"] "String") :<= []
        , C _c (ForeignRefP ["A"] "Maybe" :@ _X) :<= [C _c _X]
        ]

    instancesB = S.fromList [C _c (LocalRefP "Foo" :@ _X) :<= [C _c _X]]
