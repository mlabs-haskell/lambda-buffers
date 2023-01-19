{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{- This is my take on what our proto *should* look like.

Everything that operates on this can easily be adapted to operate on something similar, but
it will make many functions partial or introduce the need for additional error handling if
we end up abandoning the use of NonEmpty/etc.

-}
module LambdaBuffers.Common.SourceTy where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

type NEMap k v = NonEmpty (k, v)

type VarName = Text

type TyConName = Text

newtype TKind = TKind [VarName]

data TypeRef = Local TyConName | Foreign ModuleName TyConName

data Prim0 = TInt | TBool | TString

data Prim1 = TMaybe | TList

data Prim2 = TMap | TEither

data TyPrim
  = TP0 Prim0
  | TP1 Prim1
  | TP2 Prim2

data SourceTy = TyVar VarName | TApp SourceTy SourceTy | TyRef TypeRef | PrimT TyPrim

data TyDef = TyDef
  { tyDefName :: TyConName
  , tyDefKind :: TKind
  , tyDefBody :: TyBody
  }

type FieldName = Text

data Product = Record (NEMap FieldName SourceTy) | Product (NonEmpty SourceTy) | Empty

type ConstrName = Text

newtype TyBody = Sum (NonEmpty (ConstrName, Product))

type ModuleName = Text

data Module = Module
  { moduleName :: ModuleName
  , moduleDefs :: [TyDef]
  , moduleInstances :: [InstanceClause]
  }

{- InstanceClause and Constraint are just... wrong. I'll try to think of a way to fix them.
-}
data InstanceClause = InstanceClause
  { icClassName :: Text
  , icHead :: SourceTy
  , icBody :: [Constraint]
  }

data Constraint = Constraint
  { cClassName :: Text
  , cArguments :: [SourceTy]
  }

-- for writing test types
pattern PInt :: SourceTy
pattern PInt = PrimT (TP0 TInt)

pattern PBool :: SourceTy
pattern PBool = PrimT (TP0 TBool)

pattern PString :: SourceTy
pattern PString = PrimT (TP0 TString)

pattern PMaybe :: SourceTy
pattern PMaybe = PrimT (TP1 TMaybe)

pattern PList :: SourceTy
pattern PList = PrimT (TP1 TList)

pattern PMap :: SourceTy
pattern PMap = PrimT (TP2 TMap)

pattern PEither :: SourceTy
pattern PEither = PrimT (TP2 TEither)

(@) :: SourceTy -> SourceTy -> SourceTy
t1 @ t2 = t1 `TApp` t2
infixr 9 @

toNE :: [a] -> NonEmpty a
toNE = \case
  (x : xs) -> x :| xs
  _ -> error "boom"

(*=) :: a -> b -> (a, b)
a *= b = (a, b)
infixr 1 *=

testRec :: TyDef
testRec =
  TyDef
    { tyDefName = "TestStruct"
    , tyDefKind = TKind []
    , tyDefBody = Sum (("TestStruct", myRec) :| [])
    }
  where
    myRec :: Product
    myRec =
      Record $
        toNE
          [ "boolField" *= PBool
          , "intField" *= PInt
          , "maybeInt" *= PMaybe @ PInt
          ]

testProd :: TyDef
testProd = TyDef "TestProd" (TKind []) $ Sum $ toNE ["TestProd" *= prodBody]
  where
    prodBody = Product $ toNE [PBool, PInt, PMaybe `TApp` PInt]

testSum :: TyDef
testSum =
  TyDef "TestSum" (TKind []) $
    Sum $
      toNE
        [ "RecConstr" *= recBody
        , "ProdConstrMany" *= prodBody
        , "ProdConstr1" *= Product (toNE [PInt])
        , "Prod0Const0" *= Empty
        ]
  where
    prodBody = Product $ toNE [PBool, PInt, PMaybe `TApp` PInt]
    recBody =
      Record $
        toNE
          [ "boolField" *= PBool
          , "intField" *= PInt
          , "maybeInt" *= PMaybe @ PInt
          ]


testSumVars :: TyDef
testSumVars =
  TyDef "TestSum" (TKind ["a","b"]) $
    Sum $
      toNE
        [ "TestConstr1" *= recBody
        , "TestConstr2" *= prodBody
        , "TestConstr3" *= Product (toNE [PInt])
        ]
  where
    prodBody = Product $ toNE [TyVar "b", PInt, PMaybe `TApp` PInt]
    recBody =
      Record $
        toNE
          [ "aField" *= TyVar "a"
          , "intField" *= PInt
          , "maybeInt" *= PMaybe @ PInt
          ]
