{-# LANGUAGE DuplicateRecordFields #-}

module LambdaBuffers.Compiler.ProtoCompat.Types (
  SourceInfo (..),
  SourcePosition (..),
  LBName (..),
  TyName (..),
  ConstrName (..),
  ModuleName (..),
  ModuleNamePart (..),
  VarName (..),
  FieldName (..),
  ClassName (..),
  Kind (..),
  KindType (..),
  KindRefType (..),
  TyVar (..),
  Ty (..),
  TyApp (..),
  ForeignRef (..),
  LocalRef (..),
  TyRef (..),
  TyDef (..),
  TyAbs (..),
  TyArg (..),
  TyBody (..),
  Constructor (..),
  Sum (..),
  Field (..),
  Record (..),
  Tuple (..),
  Product (..),
  ClassDef (..),
  InstanceClause (..),
  Constraint (..),
  Module (..),
  CompilerInput (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

data SourceInfo = SourceInfo
  { file :: Text
  , posFrom :: SourcePosition
  , posTo :: SourcePosition
  }
  deriving stock (Show, Eq, Ord, Generic)

data SourcePosition = SourcePosition
  { column :: Int
  , row :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

-- NOTE(gnumonik): I need a "generic name" type for my template haskell, this shouldn't be used anywhere outside of that
data LBName = LBName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyName = TyName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ConstrName = ConstrName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ModuleName = ModuleName {parts :: [ModuleNamePart], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ModuleNamePart = ModuleNamePart {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data VarName = VarName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data FieldName = FieldName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ClassName = ClassName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Kind = Kind
  { kind :: KindType
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data KindType
  = KindRef KindRefType
  | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)

data KindRefType
  = KUnspecified
  | KType
  deriving stock (Show, Eq, Ord, Generic)

data TyVar = TyVar
  { varName :: VarName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Ty
  = TyVarI TyVar
  | TyAppI TyApp
  | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)

data TyApp = TyApp
  { tyFunc :: Ty
  , tyArgs :: NonEmpty Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data ForeignRef = ForeignRef
  { tyName :: TyName
  , moduleName :: ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data LocalRef = LocalRef
  { tyName :: TyName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyRef
  = LocalI LocalRef
  | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)

data TyDef = TyDef
  { tyName :: TyName
  , tyAbs :: TyAbs
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyAbs = TyAbs
  { tyArgs :: [TyArg]
  , tyBody :: TyBody
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyArg = TyArg
  { argName :: VarName
  , argKind :: Kind
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyBody
  = OpaqueI SourceInfo
  | SumI Sum
  deriving stock (Show, Eq, Ord, Generic)

data Constructor = Constructor
  { constrName :: ConstrName
  , product :: Product
  }
  deriving stock (Show, Eq, Ord, Generic)

data Sum = Sum
  { constructors :: NonEmpty Constructor
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Field = Field
  { fieldName :: FieldName
  , fieldTy :: Ty
  }
  deriving stock (Show, Eq, Ord, Generic)

data Record = Record
  { fields :: NonEmpty Field
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Tuple = Tuple
  { fields :: [Ty]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Product
  = RecordI Record
  | TupleI Tuple
  deriving stock (Show, Eq, Ord, Generic)

data ClassDef = ClassDef
  { className :: ClassName
  , classArgs :: TyArg
  , supers :: [Constraint]
  , documentation :: Text
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data InstanceClause = InstanceClause
  { className :: ClassName
  , head :: Ty
  , constraints :: [Constraint]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Constraint = Constraint
  { className :: ClassName
  , argument :: Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Module = Module
  { moduleName :: ModuleName
  , typeDefs :: [TyDef]
  , classDefs :: [ClassDef]
  , instances :: [InstanceClause]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype CompilerInput = CompilerInput {modules :: [Module]}
  deriving stock (Show, Eq, Ord, Generic)
