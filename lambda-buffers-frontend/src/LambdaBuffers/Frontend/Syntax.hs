module LambdaBuffers.Frontend.Syntax (
  Strip (..),
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
import Data.Text (Text, intercalate)

data Module info = Module
  { moduleName :: ModuleName info
  , moduleImports :: [Import info]
  , moduleTyDefs :: [TyDef info]
  , moduleInfo :: info
  }

data Import info = Import
  { importQualified :: Bool
  , importModuleName :: ModuleName info
  , imported :: Maybe [TyName info]
  , alias :: Maybe (ModuleAlias info)
  , importInfo :: info
  }

data Ty info
  = TyVar (VarName info) info
  | TyApp (Ty info) [Ty info] info
  | TyRef' (TyRef info) info

data TyDef info = TyDef
  { tyName :: TyName info
  , tyArgs :: [TyArg info]
  , tyBody :: TyBody info
  , tyDefInfo :: info
  }

data TyBody info
  = Sum [Constructor info] info
  | Opaque

data Constructor info = Constructor (ConstrName info) (Product info) info -- name this constructor body

data Product info = Product [Ty info] info

data TyArg info = TyArg Text info

data ModuleName info = ModuleName [ModuleNamePart info] info

data ModuleNamePart info = ModuleNamePart Text info

data ModuleAlias info = ModuleAlias (ModuleName info) info

data VarName info = VarName Text info

data TyName info = TyName Text info

data TyRef info = TyRef (Maybe (ModuleAlias info)) (TyName info) info

data ConstrName info = ConstrName Text info

data FieldName info = FieldName Text info

data ClassName info = ClassName Text info

class Strip f where
  strip :: f a -> f ()

instance Strip TyName where
  strip (TyName tn _) = TyName tn ()

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

instance Eq info => Eq (Import info) where
  Import iq imn is a i == Import iq' imn' is' a' i' = iq == iq' && imn == imn' && is == is' && a == a' && i == i'

instance Show info => Show (Import info) where
  show im = "import " <> if importQualified im then "qualified " else " " <> show (importModuleName im) <> maybe "" (\al -> " as " <> show al) (alias im) <> maybe "" (\imps -> " (" <> List.intercalate "," (show <$> imps) <> ")") (imported im) <> show (importInfo im)

instance Eq info => Eq (ModuleAlias info) where
  ModuleAlias mn i == ModuleAlias mn' i' = mn == mn' && i == i'
instance Ord info => Ord (ModuleAlias info) where
  ModuleAlias mn i <= ModuleAlias mn' i' = mn <= mn' && i <= i'

instance Show info => Show (ModuleAlias info) where
  show (ModuleAlias mn _) = show mn

instance Strip ModuleAlias where
  strip (ModuleAlias mn _) = ModuleAlias (strip mn) ()

instance Eq info => Eq (ModuleName info) where
  ModuleName ps i == ModuleName ps' i' = ps == ps' && i == i'

instance Ord info => Ord (ModuleName info) where
  ModuleName ps i <= ModuleName ps' i' = ps <= ps' && i <= i'

instance Strip ModuleName where
  strip (ModuleName ps _) = ModuleName (strip <$> ps) ()

instance Show info => Show (ModuleName info) where
  show (ModuleName parts _) = show $ intercalate "." [p | ModuleNamePart p _ <- parts]

instance Eq info => Eq (ModuleNamePart info) where
  ModuleNamePart p i == ModuleNamePart p' i' = p == p' && i == i'
instance Ord info => Ord (ModuleNamePart info) where
  ModuleNamePart p i <= ModuleNamePart p' i' = p <= p' && i <= i'
instance Strip ModuleNamePart where
  strip (ModuleNamePart p _) = ModuleNamePart p ()

instance Eq info => Eq (TyName info) where
  TyName tn i == TyName tn' i' = tn == tn' && i == i'

instance Ord info => Ord (TyName info) where
  TyName tn i <= TyName tn' i' = tn <= tn' && i <= i'

instance Show info => Show (TyName info) where
  show (TyName tn _) = show tn

instance Eq info => Eq (TyRef info) where
  TyRef mayModAlias tn i == TyRef mayModAlias' tn' i' = mayModAlias == mayModAlias' && tn == tn' && i == i'

instance Ord info => Ord (TyRef info) where
  TyRef mayModAlias tn i <= TyRef mayModAlias' tn' i' = mayModAlias <= mayModAlias' && tn <= tn' && i <= i'

instance Show info => Show (TyRef info) where
  show (TyRef mayModAlias tyN _) = case mayModAlias of
    Nothing -> show tyN
    Just ma -> show ma <> "." <> show tyN

instance Strip TyRef where
  strip (TyRef mayModAlias tn _) = TyRef (strip <$> mayModAlias) (strip tn) ()
