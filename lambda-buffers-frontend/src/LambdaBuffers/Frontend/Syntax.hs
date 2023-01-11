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

import Data.Text (Text, intercalate)

data Module = Module
  { moduleName :: ModuleName
  , moduleImports :: [Import]
  , moduleTyDefs :: [TyDef]
  , moduleSourceInfo :: SourceInfo
  }
  deriving stock (Eq, Show)

data Import = Import
  { importQualified :: Bool
  , importModuleName :: ModuleName
  , imported :: Maybe [TyName] -- To catch ()
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

data ModuleAlias = ModuleAlias ModuleName SourceInfo deriving stock (Eq)

instance Show ModuleAlias where
  show (ModuleAlias mn _) = show mn

data VarName = VarName Text SourceInfo deriving stock (Eq, Ord, Show)

data TyName = TyName Text SourceInfo deriving stock (Eq, Ord)

instance Show TyName where
  show (TyName tn _) = show tn

data TyRef = TyRef (Maybe ModuleAlias) TyName SourceInfo deriving stock (Eq)

instance Show TyRef where
  show (TyRef mayModAlias tyN _) = case mayModAlias of
    Nothing -> show tyN
    Just ma -> show ma <> "." <> show tyN

data ConstrName = ConstrName Text SourceInfo deriving stock (Eq, Ord, Show)

data FieldName = FieldName Text SourceInfo deriving stock (Eq, Ord, Show)

data ClassName = ClassName Text SourceInfo deriving stock (Eq, Ord, Show)

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
