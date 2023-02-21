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
  defSourceInfo,
) where

import Control.Exception (Exception)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat.SILEq (SILEq (sileq))
import Test.QuickCheck (Gen, oneof, resize, sized)
import Test.QuickCheck.Arbitrary.Generic (Arbitrary (arbitrary), GenericArbitrary (GenericArbitrary))
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Text ()

data SourceInfo = SourceInfo {file :: Text, posFrom :: SourcePosition, posTo :: SourcePosition}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary SourceInfo
  deriving anyclass (SOP.Generic)
instance SILEq SourceInfo where
  sileq = const def
instance Default SourceInfo where
  def = defSourceInfo

data SourcePosition = SourcePosition {column :: Int, row :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary SourcePosition
  deriving anyclass (SOP.Generic)
instance SILEq SourcePosition

defSourceInfo :: SourceInfo
defSourceInfo = SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)

{- | NOTE(gnumonik): I need a "generic name" type for my template haskell, this
 shouldn't be used anywhere outside of that
-}
data LBName = LBName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LBName
  deriving anyclass (SOP.Generic)

instance SILEq LBName

data TyName = TyName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyName
  deriving anyclass (SOP.Generic)
instance SILEq TyName

data ConstrName = ConstrName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ConstrName
  deriving anyclass (SOP.Generic)
instance SILEq ConstrName

data ModuleName = ModuleName {parts :: [ModuleNamePart], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ModuleName
  deriving anyclass (SOP.Generic)
instance SILEq ModuleName

data ModuleNamePart = ModuleNamePart {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ModuleNamePart
  deriving anyclass (SOP.Generic)
instance SILEq ModuleNamePart

data VarName = VarName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary VarName
  deriving anyclass (SOP.Generic)
instance SILEq VarName

data FieldName = FieldName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary FieldName
  deriving anyclass (SOP.Generic)
instance SILEq FieldName

data ClassName = ClassName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ClassName
  deriving anyclass (SOP.Generic)
instance SILEq ClassName

newtype Kind = Kind {kind :: KindType}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)
instance SILEq Kind
instance Arbitrary Kind where
  arbitrary = sized fn
    where
      fn n = Kind <$> resize n arbitrary

data KindType = KindRef KindRefType | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)
instance SILEq KindType
instance Arbitrary KindType where
  arbitrary = sized fn
    where
      fn n
        | n <= 0 = KindRef <$> arbitrary
        | otherwise = KindArrow <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary

data KindRefType = KUnspecified | KType
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindRefType
  deriving anyclass (SOP.Generic)
instance SILEq KindRefType

data TyVar = TyVar {varName :: VarName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyVar
  deriving anyclass (SOP.Generic)
instance SILEq TyVar

data Ty = TyVarI TyVar | TyAppI TyApp | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)
instance SILEq Ty
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

data TyApp = TyApp {tyFunc :: Ty, tyArgs :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyApp
  deriving anyclass (SOP.Generic)
instance SILEq TyApp

data ForeignRef = ForeignRef {tyName :: TyName, moduleName :: ModuleName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ForeignRef
  deriving anyclass (SOP.Generic)
instance SILEq ForeignRef

data LocalRef = LocalRef {tyName :: TyName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LocalRef
  deriving anyclass (SOP.Generic)
instance SILEq LocalRef

data TyRef = LocalI LocalRef | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyRef
  deriving anyclass (SOP.Generic)
instance SILEq TyRef

data TyDef = TyDef {tyName :: TyName, tyAbs :: TyAbs, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyDef
  deriving anyclass (SOP.Generic)
instance SILEq TyDef

data TyAbs = TyAbs {tyArgs :: Map VarName TyArg, tyBody :: TyBody, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyAbs
  deriving anyclass (SOP.Generic)
instance SILEq TyAbs

data TyArg = TyArg {argName :: VarName, argKind :: Kind, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyArg
  deriving anyclass (SOP.Generic)
instance SILEq TyArg

data TyBody = OpaqueI SourceInfo | SumI Sum
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyBody
  deriving anyclass (SOP.Generic)
instance SILEq TyBody

data Constructor = Constructor {constrName :: ConstrName, product :: Product}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Constructor
  deriving anyclass (SOP.Generic)
instance SILEq Constructor

data Sum = Sum {constructors :: Map ConstrName Constructor, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Sum
  deriving anyclass (SOP.Generic)
instance SILEq Sum

data Field = Field {fieldName :: FieldName, fieldTy :: Ty}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Field
  deriving anyclass (SOP.Generic)
instance SILEq Field

data Record = Record {fields :: Map FieldName Field, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Record
  deriving anyclass (SOP.Generic)
instance SILEq Record

data Tuple = Tuple {fields :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Tuple
  deriving anyclass (SOP.Generic)
instance SILEq Tuple

data Product = RecordI Record | TupleI Tuple
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary Product
  deriving anyclass (SOP.Generic)
instance SILEq Product

data ForeignClassRef = ForeignClassRef
  { className :: ClassName
  , moduleName :: ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ForeignClassRef
  deriving anyclass (SOP.Generic)
instance SILEq ForeignClassRef

data LocalClassRef = LocalClassRef {className :: ClassName, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary LocalClassRef
  deriving anyclass (SOP.Generic)
instance SILEq LocalClassRef

data TyClassRef
  = LocalCI LocalClassRef
  | ForeignCI ForeignClassRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary TyClassRef
  deriving anyclass (SOP.Generic)
instance SILEq TyClassRef

data ClassDef = ClassDef
  { className :: ClassName
  , classArgs :: TyArg
  , supers :: [Constraint]
  , documentation :: Text
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary ClassDef
  deriving anyclass (SOP.Generic)
instance SILEq ClassDef

data InstanceClause = InstanceClause
  { classRef :: TyClassRef
  , head :: Ty
  , constraints :: [Constraint]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)
instance SILEq InstanceClause

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
  deriving anyclass (SOP.Generic)
instance SILEq Constraint

data Module = Module
  { moduleName :: ModuleName
  , typeDefs :: Map TyName TyDef
  , classDefs :: Map ClassName ClassDef
  , instances :: [InstanceClause]
  , imports :: Set ModuleName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)
instance SILEq Module
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
  deriving anyclass (SOP.Generic)
instance SILEq InferenceErr
instance Exception InferenceErr

data KindCheckErr
  = InconsistentTypeErr TyDef
  | InferenceFailure TyDef InferenceErr
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindCheckErr
  deriving anyclass (SOP.Generic)
instance SILEq KindCheckErr

instance Exception KindCheckErr

newtype CompilerInput = CompilerInput {modules :: Map ModuleName Module}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (SOP.Generic)
instance SILEq CompilerInput

instance Arbitrary CompilerInput where
  arbitrary = sized fn
    where
      fn n = CompilerInput <$> resize n arbitrary

data KindCheckError
  = UnboundTyVarError TyDef TyVar ModuleName
  | UnboundTyRefError TyDef TyRef ModuleName
  | IncorrectApplicationError TyDef Kind Kind ModuleName
  | RecursiveKindError TyDef ModuleName
  | InconsistentTypeError TyDef Kind Kind ModuleName
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary KindCheckError
  deriving anyclass (SOP.Generic)
instance Exception KindCheckError
instance SILEq KindCheckError

-- | All the compiler errors.
data CompilerError
  = CompKindCheckError KindCheckError
  | InternalError Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary CompilerError
  deriving anyclass (SOP.Generic)

instance SILEq CompilerError

data CompilerResult = CompilerResult
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericArbitrary CompilerResult
  deriving anyclass (SOP.Generic)
instance SILEq CompilerResult

type CompilerOutput = Either CompilerError CompilerResult
