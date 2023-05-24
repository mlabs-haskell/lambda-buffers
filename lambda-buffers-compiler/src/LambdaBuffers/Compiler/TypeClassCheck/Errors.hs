module LambdaBuffers.Compiler.TypeClassCheck.Errors (
  importNotFoundError,
  unboundTyClassRefError,
  superClassCycleDetectedError,
  unboundTyClassRefError',
  deriveOpaqueError,
  missingRuleError,
  internalError,
  internalError',
  overlappingRulesError,
  mappendErrs,
  memptyErr,
) where

import Control.Lens ((&), (.~), (^.))
import Data.ProtoLens (Message (defMessage))
import Data.Text qualified as Text
import LambdaBuffers.Compiler.ProtoCompat.FromProto qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Utils qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

tyClassCheckError :: P.TyClassCheckError -> P.Error
tyClassCheckError err =
  defMessage
    & P.tyClassCheckErrors
      .~ [err]

importNotFoundError :: PC.ModuleName -> PC.ModuleName -> P.Error
importNotFoundError mn imp =
  tyClassCheckError $
    defMessage
      & P.importNotFoundErr . P.moduleName .~ PC.toProto mn
      & P.importNotFoundErr . P.missing .~ PC.toProto imp

-- TODO(bladyjoker): Make a consistent story with passing back errors (specific or general?).
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

unboundTyClassRefError' :: PC.ModuleName -> PC.TyClassRef -> P.Error
unboundTyClassRefError' mn = tyClassCheckError . unboundTyClassRefError mn

deriveOpaqueError :: PC.ModuleName -> PC.Constraint -> PC.Constraint -> P.Error
deriveOpaqueError locMn cstr subCstr =
  tyClassCheckError $
    defMessage
      & P.deriveOpaqueErr . P.moduleName .~ PC.toProto locMn
      & P.deriveOpaqueErr . P.constraint .~ PC.toProto cstr
      & P.deriveOpaqueErr . P.subConstraint .~ PC.toProto subCstr

missingRuleError :: PC.ModuleName -> PC.Constraint -> PC.Constraint -> P.Error
missingRuleError locMn cstr subCstr =
  tyClassCheckError $
    defMessage
      & P.missingRuleErr . P.moduleName .~ PC.toProto locMn
      & P.missingRuleErr . P.constraint .~ PC.toProto cstr
      & P.missingRuleErr . P.subConstraint .~ PC.toProto subCstr

overlappingRulesError :: PC.ModuleName -> PC.Constraint -> PC.Constraint -> [(PC.ModuleName, PC.Constraint)] -> P.Error
overlappingRulesError locMn cstr subCstr qheads =
  tyClassCheckError $
    defMessage
      & P.overlappingRulesErr . P.moduleName .~ PC.toProto locMn
      & P.overlappingRulesErr . P.constraint .~ PC.toProto cstr
      & P.overlappingRulesErr . P.subConstraint .~ PC.toProto subCstr
      & P.overlappingRulesErr . P.overlaps
        .~ [ defMessage
            & P.moduleName .~ PC.toProto mn
            & P.head .~ PC.toProto hcstr
           | (mn, hcstr) <- qheads
           ]

internalError' :: PC.ModuleName -> String -> P.Error
internalError' mn msg =
  defMessage
    & P.internalErrors
      .~ [ defMessage
            & P.msg
              .~ Text.pack
                ( "Something went unexpectedly wrong when performing type class checks in module"
                    <> "\n"
                    <> (show . PC.prettyModuleName $ mn)
                    <> "\nSince I can't show you a more informative error message I'll show you the raw error you can report at https://github.com/mlabs-haskell/lambda-buffers/issues"
                    <> "\n"
                    <> msg
                )
         ]

internalError :: PC.ModuleName -> PC.Constraint -> String -> P.Error
internalError mn cstr msg = internalError' mn ("I was trying to solve" <> "\n" <> show cstr <> "\nbut the following error occurred\n" <> msg)

-- TODO(bladyjoker): This is quite convenient, implement as Semigroup/Monoid and figure out how to use it properly.
mappendErrs :: P.Error -> P.Error -> P.Error
mappendErrs l r =
  defMessage
    & P.protoParseErrors .~ l ^. P.protoParseErrors <> r ^. P.protoParseErrors
    & P.namingErrors .~ l ^. P.namingErrors <> r ^. P.namingErrors
    & P.kindCheckErrors .~ l ^. P.kindCheckErrors <> r ^. P.kindCheckErrors
    & P.tyClassCheckErrors .~ l ^. P.tyClassCheckErrors <> r ^. P.tyClassCheckErrors
    & P.internalErrors .~ l ^. P.internalErrors <> r ^. P.internalErrors

memptyErr :: P.Error
memptyErr = defMessage
