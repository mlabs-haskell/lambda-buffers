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
import qualified Prelude

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

instance Prelude.Eq Module where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (module'name x0)) (module'name x1)))) (((Prelude.==) (module'tyDefs x0)) (module'tyDefs x1)))) (((Prelude.==) (module'classDefs x0)) (module'classDefs x1)))) (((Prelude.==) (module'ruleImports x0)) (module'ruleImports x1)))) (((Prelude.==) (module'instanceClauses x0)) (module'instanceClauses x1)))) (((Prelude.==) (module'derives x0)) (module'derives x1))) ) )
instance Prelude.Eq Module where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (module'name x0)) (module'name x1)))) (((Prelude.==) (module'tyDefs x0)) (module'tyDefs x1)))) (((Prelude.==) (module'classDefs x0)) (module'classDefs x1)))) (((Prelude.==) (module'ruleImports x0)) (module'ruleImports x1)))) (((Prelude.==) (module'instanceClauses x0)) (module'instanceClauses x1)))) (((Prelude.==) (module'derives x0)) (module'derives x1))) ) )
instance Prelude.Eq TyRef where
  (==) = (\x0 -> (\x1 -> case x0 of
                         TyRef'Local x2 -> case x1 of
                                           TyRef'Local x3 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3))
                                           TyRef'Foreign x4 x5 -> Prelude.False
                         TyRef'Foreign x6 x7 -> case x1 of
                                                TyRef'Local x8 -> Prelude.False
                                                TyRef'Foreign x9 x10 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x6) x9))) (((Prelude.==) x7) x10)) ) )
instance Prelude.Eq ModuleName where
  (==) = (\x0 -> (\x1 -> let MkModuleName x2 = x0 in let MkModuleName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq ModuleNamePart where
  (==) = (\x0 -> (\x1 -> let MkModuleNamePart x2 = x0 in let MkModuleNamePart x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq TyName where
  (==) = (\x0 -> (\x1 -> let MkTyName x2 = x0 in let MkTyName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq VarName where
  (==) = (\x0 -> (\x1 -> let MkVarName x2 = x0 in let MkVarName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )