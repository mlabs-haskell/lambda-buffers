module SourceTy where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
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

data InstanceClause = InstanceClause
  { icClassName :: Text
  , icHead :: SourceTy
  , icBody :: [Constraint]
  }

data Constraint = Constraint
  { cClassName :: Text
  , cArguments :: [SourceTy]
  }
