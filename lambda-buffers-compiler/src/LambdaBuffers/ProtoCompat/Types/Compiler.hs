module LambdaBuffers.ProtoCompat.Types.Compiler (
  CompilerError (..),
  CompilerInput (..),
  CompilerOutput,
  CompilerResult (..),
  KindCheckError (..),
  InferenceErr,
  KindCheckErr,
) where

import Control.Exception (Exception)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess, InfoLessC)
import LambdaBuffers.ProtoCompat.Types.Lang (Kind, Module, ModuleName, TyDef, TyRef, TyVar)

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

instance InfoLessC InferenceErr
instance InfoLessC KindCheckErr
instance InfoLessC CompilerInput
instance InfoLessC KindCheckError
instance InfoLessC CompilerError
instance InfoLessC CompilerResult
