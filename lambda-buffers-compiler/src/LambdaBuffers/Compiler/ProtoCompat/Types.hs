{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.ProtoCompat.Types (
  localRef2ForeignRef,
  ClassDef (..),
  ClassConstraint (..),
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
  Derive (..),
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
  InferenceErr,
  KindCheckErr,
  QClassName,
  QTyName,
) where

import Control.Exception (Exception)
import Control.Lens (Getter, to, (^.))
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map.Ordered (OMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, InfoLessC (infoLessId))

data SourceInfo = SourceInfo {file :: Text, posFrom :: SourcePosition, posTo :: SourcePosition}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data SourcePosition = SourcePosition {column :: Int, row :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Default SourceInfo where
  def = SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)

-- | Qualified type name mostly used in maintaining various contexts
type QTyName = (InfoLess ModuleName, InfoLess TyName)

-- | Qualified type class name mostly used in maintaining various contexts
type QClassName = (InfoLess ModuleName, InfoLess ClassName)

{- | NOTE(gnumonik): I need a "generic name" type for my template haskell, this
 shouldn't be used anywhere outside of that
-}
data LBName = LBName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

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

localRef2ForeignRef :: ModuleName -> Getter LocalRef ForeignRef
localRef2ForeignRef modName =
  to
    ( \lr ->
        ForeignRef
          { tyName = lr ^. #tyName
          , sourceInfo = lr ^. #sourceInfo
          , moduleName = modName
          }
    )

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

data TyBody = OpaqueI SourceInfo | SumI Sum
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

data Tuple = Tuple {fields :: [Ty], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data Product = RecordI Record | TupleI Tuple
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

data InferenceErr
  = UnboundTermErr Text
  | ImpossibleErr Text
  | UnificationErr Text
  | RecursiveSubstitutionErr Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Exception InferenceErr

data KindCheckErr
  = InconsistentTypeErr TyDef
  | InferenceFailure TyDef InferenceErr
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Exception KindCheckErr

newtype CompilerInput = CompilerInput {modules :: Map (InfoLess ModuleName) Module}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (SOP.Generic)

data KindCheckError
  = UnboundTyVarError TyDef TyVar ModuleName
  | UnboundTyRefError TyDef TyRef ModuleName
  | IncorrectApplicationError TyDef Kind Kind ModuleName
  | RecursiveKindError TyDef ModuleName
  | InconsistentTypeError TyDef Kind Kind ModuleName
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Exception KindCheckError

-- | All the compiler errors.
data CompilerError
  = CompKindCheckError KindCheckError
  | InternalError Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

data CompilerResult = CompilerResult
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

type CompilerOutput = Either CompilerError CompilerResult

-- | InfoLess instances
instance InfoLessC SourceInfo where
  infoLessId = const def

instance InfoLessC SourcePosition
instance InfoLessC LBName
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
instance InfoLessC Tuple
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
instance InfoLessC InferenceErr
instance InfoLessC KindCheckErr
instance InfoLessC CompilerInput
instance InfoLessC KindCheckError
instance InfoLessC CompilerError
instance InfoLessC CompilerResult
