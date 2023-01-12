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

import Data.List qualified as List
import Data.Text (Text)

data Module info = Module
  { moduleName :: ModuleName info
  , moduleImports :: [Import info]
  , moduleTyDefs :: [TyDef info]
  , moduleInfo :: info
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Import info = Import
  { importQualified :: Bool
  , importModuleName :: ModuleName info
  , imported :: Maybe [TyName info]
  , alias :: Maybe (ModuleAlias info)
  , importInfo :: info
  }
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)

data Ty info
  = TyVar (VarName info) info
  | TyApp (Ty info) [Ty info] info
  | TyRef' (TyRef info) info
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data TyDef info = TyDef
  { tyName :: TyName info
  , tyArgs :: [TyArg info]
  , tyBody :: TyBody info
  , tyDefInfo :: info
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data TyBody info
  = Sum [Constructor info] info
  | Opaque
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data Constructor info = Constructor (ConstrName info) (Product info) info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data Product info = Product [Ty info] info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data TyArg info = TyArg Text info deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data ModuleName info = ModuleName [ModuleNamePart info] info deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data ModuleNamePart info = ModuleNamePart Text info deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data ModuleAlias info = ModuleAlias (ModuleName info) info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data VarName info = VarName Text info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data TyName info = TyName Text info deriving stock (Eq, Ord, Functor, Foldable, Traversable)

data TyRef info = TyRef (Maybe (ModuleAlias info)) (TyName info) info deriving stock (Eq, Ord, Functor, Foldable, Traversable)

data ConstrName info = ConstrName Text info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data FieldName info = FieldName Text info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data ClassName info = ClassName Text info deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- Source information
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

-- Instances

instance Show info => Show (Import info) where
  show im = "import " <> if importQualified im then "qualified " else " " <> show (importModuleName im) <> maybe "" (\al -> " as " <> show al) (alias im) <> maybe "" (\imps -> " (" <> List.intercalate "," (show <$> imps) <> ")") (imported im) <> show (importInfo im)

instance Show info => Show (TyName info) where
  show (TyName tn _) = show tn

instance Show info => Show (TyRef info) where
  show (TyRef mayModAlias tyN _) = case mayModAlias of
    Nothing -> show tyN
    Just ma -> show ma <> "." <> show tyN
