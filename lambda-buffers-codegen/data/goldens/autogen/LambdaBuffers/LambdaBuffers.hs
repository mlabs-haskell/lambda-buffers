module LambdaBuffers.LambdaBuffers (ClassConstraint
                                   , ClassDef
                                   , ClassName
                                   , ClassRef
                                   , CompilerInput
                                   , ConstrName
                                   , Constraint
                                   , Constructor
                                   , Derive
                                   , Field
                                   , FieldName
                                   , InstanceClause
                                   , Kind
                                   , Module
                                   , ModuleName
                                   , ModuleNamePart
                                   , Product
                                   , Record
                                   , Ty
                                   , TyAbs
                                   , TyArg
                                   , TyBody
                                   , TyDef
                                   , TyName
                                   , TyRef
                                   , VarName) where

import qualified LambdaBuffers.Prelude

data ClassConstraint = MkClassConstraint { classConstraint'class :: ClassRef
                                         , classConstraint'args :: LambdaBuffers.Prelude.List TyArg}
data ClassDef = MkClassDef { classDef'name :: ClassName
                           , classDef'args :: LambdaBuffers.Prelude.List TyArg
                           , classDef'supers :: LambdaBuffers.Prelude.List ClassConstraint}
newtype ClassName = MkClassName LambdaBuffers.Prelude.Text
data ClassRef = ClassRef'Local ClassName | ClassRef'Foreign ModuleName ClassName
newtype CompilerInput = MkCompilerInput { compilerInput'modules :: LambdaBuffers.Prelude.Map ModuleName
                                                                                             Module}
newtype ConstrName = MkConstrName LambdaBuffers.Prelude.Text
data Constraint = MkConstraint { constraint'class :: ClassRef
                               , constraint'args :: LambdaBuffers.Prelude.List Ty}
data Constructor = MkConstructor { constructor'name :: ConstrName
                                 , constructor'product :: Product}
newtype Derive = MkDerive Constraint
data Field = MkField { field'name :: FieldName, field'ty :: Ty}
newtype FieldName = MkFieldName LambdaBuffers.Prelude.Text
data InstanceClause = MkInstanceClause { instanceClause'head :: Constraint
                                       , instanceClause'body :: LambdaBuffers.Prelude.List Constraint}
data Kind = Kind'Type  | Kind'Arrow Kind Kind
data Module = MkModule { module'name :: ModuleName
                       , module'tyDefs :: LambdaBuffers.Prelude.Map TyName TyDef
                       , module'classDefs :: LambdaBuffers.Prelude.Map ClassName
                                                                       ClassDef
                       , module'ruleImports :: LambdaBuffers.Prelude.Set ModuleName
                       , module'instanceClauses :: LambdaBuffers.Prelude.List InstanceClause
                       , module'derives :: LambdaBuffers.Prelude.List Derive}
newtype ModuleName = MkModuleName (LambdaBuffers.Prelude.List ModuleNamePart)
newtype ModuleNamePart = MkModuleNamePart LambdaBuffers.Prelude.Text
newtype Product = MkProduct (LambdaBuffers.Prelude.List Ty)
newtype Record = MkRecord (LambdaBuffers.Prelude.Map FieldName Field)
data Ty = Ty'App Ty Ty | Ty'Var VarName | Ty'Ref TyRef
data TyAbs = MkTyAbs { tyAbs'args :: LambdaBuffers.Prelude.List TyArg
                     , tyAbs'body :: TyBody}
data TyArg = MkTyArg { tyArg'name :: VarName, tyArg'kind :: Kind}
data TyBody = TyBody'Opaque 
               | TyBody'Sum (LambdaBuffers.Prelude.Map ConstrName Constructor)
data TyDef = MkTyDef { tyDef'name :: TyName, tyDef'abs :: TyAbs}
newtype TyName = MkTyName LambdaBuffers.Prelude.Text
data TyRef = TyRef'Local TyName | TyRef'Foreign ModuleName TyName
newtype VarName = MkVarName LambdaBuffers.Prelude.Text