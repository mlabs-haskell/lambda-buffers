{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Compiler.ProtoCompat.Types (
  ClassDef (..),
  ClassName (..),
  CompilerFailure (..),
  CompilerInput (..),
  CompilerOutput (..),
  CompilerResult (..),
  ConstrName (..),
  Constraint (..),
  Constructor (..),
  Field (..),
  FieldName (..),
  ForeignRef (..),
  InferenceErr (..),
  InstanceClause (..),
  Kind (..),
  KindRefType (..),
  KindCheckErr (..),
  KindType (..),
  LBName (..),
  LocalRef (..),
  Module (..),
  ModuleName (..),
  ModuleNamePart (..),
  Product (..),
  Record (..),
  SourceInfo (..),
  SourcePosition (..),
  Sum (..),
  Tuple (..),
  Ty (..),
  TyAbs (..),
  TyApp (..),
  TyArg (..),
  TyBody (..),
  TyDef (..),
  TyName (..),
  TyRef (..),
  TyVar (..),
  VarName (..),
  module VARS,
) where

import Control.Exception (Exception)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map qualified as M
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.KindCheck.Variable as VARS (Atom, Var)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof, sized)

data SourceInfo = SourceInfo {file :: Text, posFrom :: SourcePosition, posTo :: SourcePosition}
  deriving stock (Show, Eq, Ord, Generic)

data SourcePosition = SourcePosition {column :: Int, row :: Int}
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

newtype Kind = Kind {kind :: KindType}
  deriving stock (Show, Eq, Ord, Generic)

data KindType = KindRef KindRefType | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)

data KindRefType = KUnspecified | KType
  deriving stock (Show, Eq, Ord, Generic)

data TyVar = TyVar {varName :: VarName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Ty = TyVarI TyVar | TyAppI TyApp | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)

data TyApp = TyApp {tyFunc :: Ty, tyArgs :: NonEmpty Ty, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ForeignRef = ForeignRef {tyName :: TyName, moduleName :: ModuleName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data LocalRef = LocalRef {tyName :: TyName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyRef = LocalI LocalRef | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)

data TyDef = TyDef {tyName :: TyName, tyAbs :: TyAbs, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyAbs = TyAbs {tyArgs :: [TyArg], tyBody :: TyBody, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyArg = TyArg {argName :: VarName, argKind :: Kind, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyBody = OpaqueI SourceInfo | SumI Sum
  deriving stock (Show, Eq, Ord, Generic)

data Constructor = Constructor {constrName :: ConstrName, product :: Product}
  deriving stock (Show, Eq, Ord, Generic)

data Sum = Sum {constructors :: NonEmpty Constructor, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Field = Field {fieldName :: FieldName, fieldTy :: Ty}
  deriving stock (Show, Eq, Ord, Generic)

data Record = Record {fields :: NonEmpty Field, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Tuple = Tuple {fields :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Product = RecordI Record | TupleI Tuple
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

data InferenceErr
  = UnboundTermErr Text
  | ImpossibleErr Text
  | UnificationErr Text
  | RecursiveSubstitutionErr Text
  deriving stock (Show, Eq, Ord, Generic)

instance Exception InferenceErr

data KindCheckErr
  = InconsistentTypeErr TyDef
  | InferenceFailure TyDef InferenceErr
  deriving stock (Show, Eq, Ord, Generic)

instance Exception KindCheckErr

newtype CompilerInput = CompilerInput {modules :: [Module]}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Monoid, Semigroup, Arbitrary)

newtype CompilerOutput = CompilerOutput {typeDefs :: M.Map Var Kind}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Arbitrary)

newtype CompilerFailure = KCErr KindCheckErr
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Arbitrary)

data CompilerResult
  = RCompilerFailure CompilerFailure
  | RCompilerOutput CompilerOutput
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary SourceInfo where
  arbitrary = SourceInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourcePosition where
  arbitrary = SourcePosition <$> arbitrary <*> arbitrary

instance Arbitrary LBName where
  arbitrary = LBName <$> arbitrary <*> arbitrary

instance Arbitrary TyName where
  arbitrary = TyName <$> arbitrary <*> arbitrary

instance Arbitrary ConstrName where
  arbitrary = ConstrName <$> arbitrary <*> arbitrary

instance Arbitrary ModuleName where
  arbitrary = ModuleName <$> arbitrary <*> arbitrary

instance Arbitrary ModuleNamePart where
  arbitrary = ModuleNamePart <$> arbitrary <*> arbitrary

instance Arbitrary VarName where
  arbitrary = VarName <$> arbitrary <*> arbitrary

instance Arbitrary FieldName where
  arbitrary = FieldName <$> arbitrary <*> arbitrary

instance Arbitrary ClassName where
  arbitrary = ClassName <$> arbitrary <*> arbitrary

instance Arbitrary Kind where
  arbitrary = Kind <$> arbitrary

instance Arbitrary KindType where
  arbitrary = oneof [KindRef <$> arbitrary, KindArrow <$> arbitrary <*> arbitrary]

instance Arbitrary KindRefType where
  arbitrary = oneof [pure KUnspecified, pure KType]

instance Arbitrary TyVar where
  arbitrary = TyVar <$> arbitrary <*> arbitrary

instance Arbitrary Ty where
  arbitrary = oneof [TyVarI <$> arbitrary, TyAppI <$> arbitrary, TyRefI <$> arbitrary]

instance Arbitrary TyApp where
  arbitrary = TyApp <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ForeignRef where
  arbitrary = ForeignRef <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LocalRef where
  arbitrary = LocalRef <$> arbitrary <*> arbitrary

instance Arbitrary TyRef where
  arbitrary = oneof [LocalI <$> arbitrary, ForeignI <$> arbitrary]

instance Arbitrary TyDef where
  arbitrary = TyDef <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TyAbs where
  arbitrary = TyAbs <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TyArg where
  arbitrary = TyArg <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TyBody where
  arbitrary = oneof [OpaqueI <$> arbitrary, SumI <$> arbitrary]

instance Arbitrary Constructor where
  arbitrary = Constructor <$> arbitrary <*> arbitrary

instance Arbitrary Sum where
  arbitrary = Sum <$> arbitrary <*> arbitrary

instance Arbitrary Field where
  arbitrary = Field <$> arbitrary <*> arbitrary

instance Arbitrary Record where
  arbitrary = Record <$> arbitrary <*> arbitrary

instance Arbitrary Tuple where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance Arbitrary Product where
  arbitrary = oneof [RecordI <$> arbitrary, TupleI <$> arbitrary]

instance Arbitrary ClassDef where
  arbitrary = ClassDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InstanceClause where
  arbitrary = InstanceClause <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Constraint where
  arbitrary = Constraint <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Module where
  arbitrary = Module <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InferenceErr where
  arbitrary =
    oneof
      [ UnboundTermErr <$> arbitrary
      , ImpossibleErr <$> arbitrary
      , UnificationErr <$> arbitrary
      , RecursiveSubstitutionErr <$> arbitrary
      ]

instance Arbitrary KindCheckErr where
  arbitrary =
    oneof
      [ InconsistentTypeErr <$> arbitrary
      , InferenceFailure <$> arbitrary <*> arbitrary
      ]

instance Arbitrary CompilerResult where
  arbitrary =
    oneof
      [ RCompilerFailure <$> arbitrary
      , RCompilerOutput <$> arbitrary
      ]

-- Orphan Instances

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = sized f
    where
      f :: (Num t, Ord t) => t -> Gen (NonEmpty a)
      f n
        | n <= 0 = do
            x <- arbitrary @a
            pure $ x :| []
        | otherwise = do
            x <- arbitrary
            xs <- f (n - 1)
            pure $ x <| xs

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
