module LambdaBuffers.Compiler.TypeClassCheck.Errors (importNotFoundError, unboundTyClassRefError, superClassCycleDetectedError, unboundTyClassRefError') where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.ProtoCompat.FromProto qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

tyClassCheckError :: P.TyClassCheckError -> P.CompilerError
tyClassCheckError err =
  defMessage
    & P.tyClassCheckErrors
      .~ [err]

importNotFoundError :: PC.ModuleName -> PC.ModuleName -> P.CompilerError
importNotFoundError mn imp =
  tyClassCheckError $
    defMessage
      & P.importNotFoundErr . P.moduleName .~ PC.toProto mn
      & P.importNotFoundErr . P.missing .~ PC.toProto imp

-- TODO(bladyjoker): Make a consistent story with error reporting.
superClassCycleDetectedError :: PC.ModuleName -> PC.ClassName -> [PC.TyClassRef] -> P.TyClassCheckError
superClassCycleDetectedError mn currcn trace =
  defMessage
    & P.superclassCycleErr . P.moduleName .~ PC.toProto mn
    & P.superclassCycleErr . P.className .~ PC.toProto currcn
    & P.superclassCycleErr . P.cycledClassRefs .~ (PC.toProto <$> trace)

unboundTyClassRefError :: PC.ModuleName -> PC.TyClassRef -> P.TyClassCheckError
unboundTyClassRefError mn cr =
  defMessage
    & P.unboundClassRefErr . P.moduleName .~ PC.toProto mn
    & P.unboundClassRefErr . P.classRef .~ PC.toProto cr

unboundTyClassRefError' :: PC.ModuleName -> PC.TyClassRef -> P.CompilerError
unboundTyClassRefError' mn = tyClassCheckError . unboundTyClassRefError mn
