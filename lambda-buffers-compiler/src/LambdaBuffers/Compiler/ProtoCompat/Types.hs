{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
--  this is needed so the deriving via can generate Arbitrary instances for data
--  definitions with more than 4 constructors
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module LambdaBuffers.Compiler.ProtoCompat.Types (
  ClassDef (..),
  ClassName (..),
  CompilerError (..),
  CompilerInput (..),
  CompilerOutput,
  CompilerResult (..),
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
  KindCheckError (..),
  KindType (..),
  LBName (..),
  LocalRef (..),
  LocalClassRef (..),
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
  TyClassRef (..),
  TyDef (..),
  TyName (..),
  TyRef (..),
  TyVar (..),
  VarName (..),
  module VARS,
) where

-- for NonEmpty
import Control.Exception (Exception)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.KindCheck.Variable as VARS (Atom, Variable)
import Test.QuickCheck (Gen, oneof, resize, sized)
import Test.QuickCheck.Arbitrary.Generic (Arbitrary (arbitrary), GenericArbitrary (GenericArbitrary))
import Test.QuickCheck.Instances.Semigroup ()

data SourceInfo = SourceInfo {file :: Text, posFrom :: SourcePosition, posTo :: SourcePosition}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary SourceInfo

data SourcePosition = SourcePosition {column :: Int, row :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary SourcePosition

{- | NOTE(gnumonik): I need a "generic name" type for my template haskell, this
 shouldn't be used anywhere outside of that
-}
data LBName = LBName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LBName

data TyName = TyName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyName

data ConstrName = ConstrName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ConstrName

data ModuleName = ModuleName {parts :: [ModuleNamePart], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ModuleName

data ModuleNamePart = ModuleNamePart {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ModuleNamePart

data VarName = VarName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary VarName

data FieldName = FieldName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary FieldName

data ClassName = ClassName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ClassName

newtype Kind = Kind {kind :: KindType}
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary Kind where
  arbitrary = sized fn
    where
      fn n = Kind <$> resize n arbitrary

data KindType = KindRef KindRefType | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary KindType where
  arbitrary = sized fn
    where
      fn n
        | n <= 0 = KindRef <$> arbitrary
        | otherwise = KindArrow <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary

data KindRefType = KUnspecified | KType
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindRefType

data TyVar = TyVar {varName :: VarName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyVar

data Ty = TyVarI TyVar | TyAppI TyApp | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary Ty where
  arbitrary = sized fn
    where
      fn :: (Num a, Ord a) => a -> Gen Ty
      fn n
        | n <= 0 = TyRefI <$> arbitrary
        | otherwise =
            oneof
              [ TyVarI <$> arbitrary
              , TyAppI <$> arbitrary
              , TyRefI <$> arbitrary
              ]

data TyApp = TyApp {tyFunc :: Ty, tyArgs :: NonEmpty Ty, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyApp

data ForeignRef = ForeignRef {tyName :: TyName, moduleName :: ModuleName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ForeignRef

data LocalRef = LocalRef {tyName :: TyName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LocalRef

data TyRef = LocalI LocalRef | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyRef

data TyDef = TyDef {tyName :: TyName, tyAbs :: TyAbs, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyDef

data TyAbs = TyAbs {tyArgs :: [TyArg], tyBody :: TyBody, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyAbs

data TyArg = TyArg {argName :: VarName, argKind :: Kind, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyArg

data TyBody = OpaqueI SourceInfo | SumI Sum
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyBody

data Constructor = Constructor {constrName :: ConstrName, product :: Product}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Constructor

data Sum = Sum {constructors :: NonEmpty Constructor, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Sum

data Field = Field {fieldName :: FieldName, fieldTy :: Ty}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Field

data Record = Record {fields :: NonEmpty Field, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Record

data Tuple = Tuple {fields :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Tuple

data Product = RecordI Record | TupleI Tuple
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Product

data ForeignClassRef = ForeignClassRef
  { className :: ClassName
  , moduleName :: ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ForeignClassRef

data LocalClassRef = LocalClassRef {className :: ClassName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LocalClassRef

data TyClassRef
  = LocalCI LocalClassRef
  | ForeignCI ForeignClassRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyClassRef

data ClassDef = ClassDef
  { className :: ClassName
  , classArgs :: TyArg
  , supers :: [Constraint]
  , documentation :: Text
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ClassDef

data InstanceClause = InstanceClause
  { classRef :: TyClassRef
  , head :: Ty
  , constraints :: [Constraint]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary InstanceClause where
  arbitrary = sized fn
    where
      fn n =
        InstanceClause
          <$> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary

data Constraint = Constraint
  { classRef :: TyClassRef
  , argument :: Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Constraint

data Module = Module
  { moduleName :: ModuleName
  , typeDefs :: [TyDef]
  , classDefs :: [ClassDef]
  , instances :: [InstanceClause]
  , imports :: [ModuleName]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Arbitrary Module where
  arbitrary = sized fn
    where
      fn n =
        Module
          <$> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary
          <*> resize n arbitrary

data InferenceErr
  = UnboundTermErr Text
  | ImpossibleErr Text
  | UnificationErr Text
  | RecursiveSubstitutionErr Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary InferenceErr

instance Exception InferenceErr

data KindCheckErr
  = InconsistentTypeErr TyDef
  | InferenceFailure TyDef InferenceErr
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindCheckErr

instance Exception KindCheckErr

newtype CompilerInput = CompilerInput {modules :: [Module]}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Monoid, Semigroup)

instance Arbitrary CompilerInput where
  arbitrary = sized fn
    where
      fn n = CompilerInput <$> resize n arbitrary

data KindCheckError
  = UnboundTermError TyName VarName
  | IncorrectApplicationError TyName Kind Kind
  | RecursiveKindError TyName
  | InconsistentTypeError TyName Kind Kind
  | MultipleTyDefError ModuleName [TyDef]
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindCheckError
instance Exception KindCheckError

-- | All the compiler errors.
data CompilerError
  = CompKindCheckError KindCheckError
  | InternalError Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary CompilerError

data CompilerResult = CompilerResult
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary CompilerResult

type CompilerOutput = Either CompilerError CompilerResult
