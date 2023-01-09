module LambdaBuffers.Frontend.Syntax (
  Module (..),
  Import (..),
  Ty (..),
  TyRef (..),
  TyDef (..),
  TyBody (..),
  Constructor (..),
  Product (..),
  TyArg (..),
  ModuleName (..),
  ModuleAlias (..),
  ModuleNamePart (..),
  TyName (..),
  VarName (..),
  ConstrName (..),
  FieldName (..),
  ClassName (..),
  SourceInfo (..),
  SourcePos (..),
  testSourceInfo,
) where

import Data.Foldable (Foldable (fold))
import Data.List qualified as List
import Data.Text (Text, intercalate)

data Module = Module
  { moduleName :: ModuleName
  , moduleImports :: [Import]
  , moduleTyDefs :: [TyDef]
  , moduleSourceInfog :: SourceInfo
  }
  deriving stock (Eq, Show)

data Import = Import
  { importQualified :: Bool
  , importModuleName :: ModuleName
  , imported :: [TyName]
  , alias :: Maybe ModuleAlias
  , importSourceInfo :: SourceInfo
  }
  deriving stock (Eq, Show)

data Ty
  = TyVar VarName SourceInfo
  | TyApp Ty [Ty] SourceInfo
  | TyRef' TyRef SourceInfo
  deriving stock (Eq, Show)

data TyDef = TyDef
  { tyName :: TyName
  , tyArgs :: [TyArg]
  , tyBody :: TyBody
  , tyDefSourceInfo :: SourceInfo
  }
  deriving stock (Eq, Show)

data TyBody
  = Sum [Constructor] SourceInfo
  | Opaque
  deriving stock (Eq, Show)

data Constructor = Constructor ConstrName Product SourceInfo deriving stock (Eq, Show) -- name this constructor body

data Product = Product [Ty] SourceInfo deriving stock (Eq, Show)

data TyArg = TyArg Text SourceInfo deriving stock (Eq, Show)

data ModuleName = ModuleName [ModuleNamePart] SourceInfo deriving stock (Eq, Ord)
instance Show ModuleName where
  show (ModuleName parts _) = show $ intercalate "." [p | ModuleNamePart p _ <- parts]
data ModuleNamePart = ModuleNamePart Text SourceInfo deriving stock (Eq, Ord, Show)
data ModuleAlias = ModuleAlias Text SourceInfo deriving stock (Eq, Show)
data VarName = VarName Text SourceInfo deriving stock (Eq, Show)
data TyName = TyName Text SourceInfo deriving stock (Eq, Show)
data TyRef = TyRef (Maybe ModuleAlias) TyName SourceInfo deriving stock (Eq, Show)
data ConstrName = ConstrName Text SourceInfo deriving stock (Eq, Show)
data FieldName = FieldName Text SourceInfo deriving stock (Eq, Show)
data ClassName = ClassName Text SourceInfo deriving stock (Eq, Show)

data SourceInfo = SourceInfo
  { filename :: Text
  , from :: SourcePos
  , to :: SourcePos
  }
  deriving stock (Eq, Ord)

instance Show SourceInfo where
  show (SourceInfo filename pos pos') = show filename <> ":" <> "(" <> show pos <> ")-(" <> show pos' <> ")"

data SourcePos = SourcePos
  { row :: Int
  , column :: Int
  }
  deriving stock (Eq, Ord)

instance Show SourcePos where
  show (SourcePos r c) = show r <> ":" <> show c

testSourceInfo :: SourceInfo
testSourceInfo = SourceInfo "test" (SourcePos 0 0) (SourcePos 0 0)
