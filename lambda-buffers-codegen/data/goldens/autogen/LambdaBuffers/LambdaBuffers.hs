module LambdaBuffers.LambdaBuffers (ClassConstraint(..)
                                   , ClassDef(..)
                                   , ClassName(..)
                                   , ClassRef(..)
                                   , CompilerInput(..)
                                   , ConstrName(..)
                                   , Constraint(..)
                                   , Constructor(..)
                                   , Derive(..)
                                   , Field(..)
                                   , FieldName(..)
                                   , InstanceClause(..)
                                   , Kind(..)
                                   , Module(..)
                                   , ModuleName(..)
                                   , ModuleNamePart(..)
                                   , Product(..)
                                   , Record(..)
                                   , Ty(..)
                                   , TyAbs(..)
                                   , TyArg(..)
                                   , TyBody(..)
                                   , TyDef(..)
                                   , TyName(..)
                                   , TyRef(..)
                                   , VarName(..)) where

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

instance Prelude.Eq Kind where
  (==) = (\x0 -> (\x1 -> case x0 of
                         Kind'Type  -> case x1 of
                                       Kind'Type  -> Prelude.True
                                       Kind'Arrow x2 x3 -> Prelude.False
                         Kind'Arrow x4 x5 -> case x1 of
                                             Kind'Type  -> Prelude.False
                                             Kind'Arrow x6 x7 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x4) x6))) (((Prelude.==) x5) x7)) ) )
instance Prelude.Eq Ty where
  (==) = (\x0 -> (\x1 -> case x0 of
                         Ty'App x2 x3 -> case x1 of
                                         Ty'App x4 x5 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x4))) (((Prelude.==) x3) x5))
                                         Ty'Var x6 -> Prelude.False
                                         Ty'Ref x7 -> Prelude.False
                         Ty'Var x8 -> case x1 of
                                      Ty'App x9 x10 -> Prelude.False
                                      Ty'Var x11 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x8) x11))
                                      Ty'Ref x12 -> Prelude.False
                         Ty'Ref x13 -> case x1 of
                                       Ty'App x14 x15 -> Prelude.False
                                       Ty'Var x16 -> Prelude.False
                                       Ty'Ref x17 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x13) x17)) ) )
instance Prelude.Eq TyAbs where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (tyAbs'args x0)) (tyAbs'args x1)))) (((Prelude.==) (tyAbs'body x0)) (tyAbs'body x1))) ) )
instance Prelude.Eq TyArg where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (tyArg'name x0)) (tyArg'name x1)))) (((Prelude.==) (tyArg'kind x0)) (tyArg'kind x1))) ) )
instance Prelude.Eq TyDef where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (tyDef'name x0)) (tyDef'name x1)))) (((Prelude.==) (tyDef'abs x0)) (tyDef'abs x1))) ) )
instance Prelude.Eq TyBody where
  (==) = (\x0 -> (\x1 -> case x0 of
                         TyBody'Opaque  -> case x1 of
                                           TyBody'Opaque  -> Prelude.True
                                           TyBody'Sum x2 -> Prelude.False
                         TyBody'Sum x3 -> case x1 of
                                          TyBody'Opaque  -> Prelude.False
                                          TyBody'Sum x4 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x3) x4)) ) )
instance Prelude.Eq Constructor where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (constructor'name x0)) (constructor'name x1)))) (((Prelude.==) (constructor'product x0)) (constructor'product x1))) ) )
instance Prelude.Eq Product where
  (==) = (\x0 -> (\x1 -> let MkProduct x2 = x0 in let MkProduct x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq Record where
  (==) = (\x0 -> (\x1 -> let MkRecord x2 = x0 in let MkRecord x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq Field where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (field'name x0)) (field'name x1)))) (((Prelude.==) (field'ty x0)) (field'ty x1))) ) )
instance Prelude.Eq TyRef where
  (==) = (\x0 -> (\x1 -> case x0 of
                         TyRef'Local x2 -> case x1 of
                                           TyRef'Local x3 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3))
                                           TyRef'Foreign x4 x5 -> Prelude.False
                         TyRef'Foreign x6 x7 -> case x1 of
                                                TyRef'Local x8 -> Prelude.False
                                                TyRef'Foreign x9 x10 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x6) x9))) (((Prelude.==) x7) x10)) ) )
instance Prelude.Eq ClassDef where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (classDef'name x0)) (classDef'name x1)))) (((Prelude.==) (classDef'args x0)) (classDef'args x1)))) (((Prelude.==) (classDef'supers x0)) (classDef'supers x1))) ) )
instance Prelude.Eq ClassConstraint where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (classConstraint'class x0)) (classConstraint'class x1)))) (((Prelude.==) (classConstraint'args x0)) (classConstraint'args x1))) ) )
instance Prelude.Eq InstanceClause where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (instanceClause'head x0)) (instanceClause'head x1)))) (((Prelude.==) (instanceClause'body x0)) (instanceClause'body x1))) ) )
instance Prelude.Eq Derive where
  (==) = (\x0 -> (\x1 -> let MkDerive x2 = x0 in let MkDerive x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq Constraint where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (constraint'class x0)) (constraint'class x1)))) (((Prelude.==) (constraint'args x0)) (constraint'args x1))) ) )
instance Prelude.Eq ClassRef where
  (==) = (\x0 -> (\x1 -> case x0 of
                         ClassRef'Local x2 -> case x1 of
                                              ClassRef'Local x3 -> (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3))
                                              ClassRef'Foreign x4 x5 -> Prelude.False
                         ClassRef'Foreign x6 x7 -> case x1 of
                                                   ClassRef'Local x8 -> Prelude.False
                                                   ClassRef'Foreign x9 x10 -> (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x6) x9))) (((Prelude.==) x7) x10)) ) )
instance Prelude.Eq Module where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) (module'name x0)) (module'name x1)))) (((Prelude.==) (module'tyDefs x0)) (module'tyDefs x1)))) (((Prelude.==) (module'classDefs x0)) (module'classDefs x1)))) (((Prelude.==) (module'ruleImports x0)) (module'ruleImports x1)))) (((Prelude.==) (module'instanceClauses x0)) (module'instanceClauses x1)))) (((Prelude.==) (module'derives x0)) (module'derives x1))) ) )
instance Prelude.Eq CompilerInput where
  (==) = (\x0 -> (\x1 -> (((Prelude.&&) Prelude.True) (((Prelude.==) (compilerInput'modules x0)) (compilerInput'modules x1))) ) )
instance Prelude.Eq TyName where
  (==) = (\x0 -> (\x1 -> let MkTyName x2 = x0 in let MkTyName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq VarName where
  (==) = (\x0 -> (\x1 -> let MkVarName x2 = x0 in let MkVarName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq ConstrName where
  (==) = (\x0 -> (\x1 -> let MkConstrName x2 = x0 in let MkConstrName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq FieldName where
  (==) = (\x0 -> (\x1 -> let MkFieldName x2 = x0 in let MkFieldName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq ModuleName where
  (==) = (\x0 -> (\x1 -> let MkModuleName x2 = x0 in let MkModuleName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq ModuleNamePart where
  (==) = (\x0 -> (\x1 -> let MkModuleNamePart x2 = x0 in let MkModuleNamePart x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )
instance Prelude.Eq ClassName where
  (==) = (\x0 -> (\x1 -> let MkClassName x2 = x0 in let MkClassName x3 = x1 in (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x3)) ) )