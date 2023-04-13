{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Source where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

type VarName = Text

type TyConName = Text

newtype TKind = TKind [VarName]

data TypeRef = Local TyConName | Foreign ModuleName TyConName

data Ty = TyVar VarName | TCon TKind | TApp Ty Ty | TyRef TypeRef

data TyDef = TyDef
  { tyDefName :: TyConName
  , tyDefKind :: TKind
  , tyDefBody :: TyBody
  }

data FieldName = Text

data Product = Record (Map FieldName Ty) | Product [Ty] | Empty

type ConstrName = Text

data TyBody = Opaque | Sum (Map ConstrName Product)

type ModuleName = Text

data Module = Module
  { moduleName :: ModuleName
  , moduleDefs :: [TyDef]
  , moduleInstances :: [InstanceClause]
  }

data InstanceClause = InstanceClause
  { icClassName :: Text
  , icHead :: Ty
  , icBody :: [Constraint]
  }

data Constraint = Constraint
  { cClassName :: Text
  , cArguments :: [Ty]
  }

opaque tyConName args = TyDef tyConName (TKind args) Opaque

dt tyConName args constructors = TyDef tyConName (TKind args) (Sum $ Map.fromList [(cname, prod) | (cname, prod) <- constructors])

var = TyVar

vars vnames = Product $ TyVar <$> vnames

nullary cn = (cn, Empty)

app = TApp

ty = TyRef . Local

prod = Product

recrd = Record

fty m = TyRef . Foreign (moduleName m)

ftyMyMod = fty myModule

jsonInstance = InstanceClause "JSON"

jsonConstr = Constraint "JSON"

-- | User defs
myModule =
  Module
    { moduleName = "MyModule"
    , moduleDefs =
        [ opaque "Integer" []
        , opaque "Set" ["a"]
        , opaque "Map" ["k", "v"]
        , dt "Maybe" ["a"] [("Just", prod [var "a"]), nullary "Nothing"]
        , dt
            "MyType"
            ["a", "b"]
            [("Foo", prod [ty "Maybe" `app` ty "Map" `app` var "a" `app` (ty "Set" `app` var "b")])]
        ]
    , moduleInstances =
        [ jsonInstance (ty "Maybe" `app` var "a") [jsonConstr [var "a"]]
        ]
    }

anotherModule =
  Module
    { moduleName = "AnotherModule"
    , moduleDefs =
        [ dt
            "AnotherType"
            ["a", "b"]
            [("Bar", prod [ftyMyMod "Maybe" `app` ftyMyMod "Map" `app` var "a" `app` (ftyMyMod "Set" `app` var "b")])]
        ]
    , moduleInstances =
        [ jsonInstance (ftyMyMod "Maybe" `app` var "a") [jsonConstr [var "a"]]
        ]
    }

main = do
  putStrLn "Hello"
  putStrLn "World"
