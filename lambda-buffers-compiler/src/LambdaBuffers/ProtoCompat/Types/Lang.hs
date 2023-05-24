{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.ProtoCompat.Types.Lang (
  ClassDef (..),
  ClassConstraint (..),
  ClassName (..),
  ConstrName (..),
  Constraint (..),
  Constructor (..),
  Field (..),
  FieldName (..),
  ForeignRef (..),
  ForeignClassRef (..),
  InstanceClause (..),
  Kind (..),
  KindRefType (..),
  KindType (..),
  LocalRef (..),
  LocalClassRef (..),
  Derive (..),
  Module (..),
  ModuleName (..),
  ModuleNamePart (..),
  Product (..),
  Record (..),
  SourceInfo (..),
  SourcePosition (..),
  Sum (..),
  Ty (..),
  TyAbs (..),
  TyApp (..),
  TyArg (..),
  TyBody (..),
  TyClassRef (..),
  TyDef (..),
  TyName (..),
  TyRef (..),
  TyVar (..),
  VarName (..),
) where

import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map.Ordered (OMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess, InfoLessC (infoLessId))

data SourceInfo = SourceInfo {file :: Text, posFrom :: SourcePosition, posTo :: SourcePosition}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data SourcePosition = SourcePosition {column :: Int, row :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Default SourceInfo where
  def = SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)

data TyName = TyName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ConstrName = ConstrName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ModuleName = ModuleName {parts :: [ModuleNamePart], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ModuleNamePart = ModuleNamePart {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data VarName = VarName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data FieldName = FieldName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ClassName = ClassName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

newtype Kind = Kind {kind :: KindType}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data KindType = KindRef KindRefType | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data KindRefType = KUnspecified | KType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

newtype TyVar = TyVar {varName :: VarName}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Ty = TyVarI TyVar | TyAppI TyApp | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyApp = TyApp {tyFunc :: Ty, tyArgs :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ForeignRef = ForeignRef {tyName :: TyName, moduleName :: ModuleName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data LocalRef = LocalRef {tyName :: TyName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyRef = LocalI LocalRef | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyDef = TyDef {tyName :: TyName, tyAbs :: TyAbs, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyAbs = TyAbs {tyArgs :: OMap (InfoLess VarName) TyArg, tyBody :: TyBody, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyArg = TyArg {argName :: VarName, argKind :: Kind, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyBody = OpaqueI SourceInfo | SumI Sum | ProductI Product | RecordI Record
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Constructor = Constructor {constrName :: ConstrName, product :: Product}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Sum = Sum {constructors :: OMap (InfoLess ConstrName) Constructor, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Field = Field {fieldName :: FieldName, fieldTy :: Ty}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Record = Record {fields :: OMap (InfoLess FieldName) Field, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Product = Product {fields :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ForeignClassRef = ForeignClassRef
  { className :: ClassName
  , moduleName :: ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data LocalClassRef = LocalClassRef {className :: ClassName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data TyClassRef
  = LocalCI LocalClassRef
  | ForeignCI ForeignClassRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ClassDef = ClassDef
  { className :: ClassName
  , classArgs :: TyArg
  , supers :: [ClassConstraint]
  , documentation :: Text
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data ClassConstraint = ClassConstraint
  { classRef :: TyClassRef
  , args :: TyVar
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

newtype Derive = Derive
  { constraint :: Constraint
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data InstanceClause = InstanceClause
  { head :: Constraint
  , constraints :: [Constraint]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Constraint = Constraint
  { classRef :: TyClassRef
  , argument :: Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Module = Module
  { moduleName :: ModuleName
  , typeDefs :: Map (InfoLess TyName) TyDef
  , classDefs :: Map (InfoLess ClassName) ClassDef
  , instances :: [InstanceClause]
  , derives :: [Derive]
  , imports :: Map (InfoLess ModuleName) ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

-- | InfoLess instances
instance InfoLessC SourceInfo where
  infoLessId = const def

instance InfoLessC SourcePosition
instance InfoLessC TyName
instance InfoLessC ConstrName
instance InfoLessC ModuleName
instance InfoLessC ModuleNamePart
instance InfoLessC VarName
instance InfoLessC FieldName
instance InfoLessC ClassName
instance InfoLessC Kind
instance InfoLessC KindType
instance InfoLessC KindRefType
instance InfoLessC TyVar
instance InfoLessC Ty
instance InfoLessC TyApp
instance InfoLessC ForeignRef
instance InfoLessC LocalRef
instance InfoLessC TyRef
instance InfoLessC TyDef
instance InfoLessC TyAbs
instance InfoLessC TyArg
instance InfoLessC TyBody
instance InfoLessC Constructor
instance InfoLessC Sum
instance InfoLessC Field
instance InfoLessC Record
instance InfoLessC Product
instance InfoLessC ForeignClassRef
instance InfoLessC LocalClassRef
instance InfoLessC TyClassRef
instance InfoLessC ClassDef
instance InfoLessC ClassConstraint
instance InfoLessC InstanceClause
instance InfoLessC Derive
instance InfoLessC Constraint
instance InfoLessC Module
