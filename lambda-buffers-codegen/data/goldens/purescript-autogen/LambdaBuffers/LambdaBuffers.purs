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

import LambdaBuffers.Prelude as LambdaBuffers.Prelude
import Data.BigInt as Data.BigInt
import Data.Newtype as Data.Newtype
import Prelude as Prelude

newtype ClassConstraint = MkClassConstraint { class :: ClassRef
                                            , args :: LambdaBuffers.Prelude.List TyArg}
derive instance Data.Newtype.Newtype ClassConstraint _
newtype ClassDef = MkClassDef { name :: ClassName
                              , args :: LambdaBuffers.Prelude.List TyArg
                              , supers :: LambdaBuffers.Prelude.List ClassConstraint}
derive instance Data.Newtype.Newtype ClassDef _
newtype ClassName = MkClassName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ClassName _
data ClassRef = ClassRef'Local ClassName | ClassRef'Foreign ModuleName ClassName

newtype CompilerInput = MkCompilerInput { modules :: LambdaBuffers.Prelude.Map ModuleName
                                                                               Module}
derive instance Data.Newtype.Newtype CompilerInput _
newtype ConstrName = MkConstrName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ConstrName _
newtype Constraint = MkConstraint { class :: ClassRef
                                  , args :: LambdaBuffers.Prelude.List Ty}
derive instance Data.Newtype.Newtype Constraint _
newtype Constructor = MkConstructor { name :: ConstrName, product :: Product}
derive instance Data.Newtype.Newtype Constructor _
newtype Derive = MkDerive Constraint
derive instance Data.Newtype.Newtype Derive _
newtype Field = MkField { name :: FieldName, ty :: Ty}
derive instance Data.Newtype.Newtype Field _
newtype FieldName = MkFieldName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype FieldName _
newtype InstanceClause = MkInstanceClause { head :: Constraint
                                          , body :: LambdaBuffers.Prelude.List Constraint}
derive instance Data.Newtype.Newtype InstanceClause _
data Kind = Kind'Type  | Kind'Arrow Kind Kind

newtype Module = MkModule { name :: ModuleName
                          , tyDefs :: LambdaBuffers.Prelude.Map TyName TyDef
                          , classDefs :: LambdaBuffers.Prelude.Map ClassName
                                                                   ClassDef
                          , ruleImports :: LambdaBuffers.Prelude.Set ModuleName
                          , instanceClauses :: LambdaBuffers.Prelude.List InstanceClause
                          , derives :: LambdaBuffers.Prelude.List Derive}
derive instance Data.Newtype.Newtype Module _
newtype ModuleName = MkModuleName (LambdaBuffers.Prelude.List ModuleNamePart)
derive instance Data.Newtype.Newtype ModuleName _
newtype ModuleNamePart = MkModuleNamePart LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ModuleNamePart _
newtype Product = MkProduct (LambdaBuffers.Prelude.List Ty)
derive instance Data.Newtype.Newtype Product _
newtype Record = MkRecord (LambdaBuffers.Prelude.Map FieldName Field)
derive instance Data.Newtype.Newtype Record _
data Ty = Ty'App Ty Ty | Ty'Var VarName | Ty'Ref TyRef

newtype TyAbs = MkTyAbs { args :: LambdaBuffers.Prelude.List TyArg
                        , body :: TyBody}
derive instance Data.Newtype.Newtype TyAbs _
newtype TyArg = MkTyArg { name :: VarName, kind :: Kind}
derive instance Data.Newtype.Newtype TyArg _
data TyBody = TyBody'Opaque 
               | TyBody'Sum (LambdaBuffers.Prelude.Map ConstrName Constructor)

newtype TyDef = MkTyDef { name :: TyName, abs :: TyAbs}
derive instance Data.Newtype.Newtype TyDef _
newtype TyName = MkTyName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype TyName _
data TyRef = TyRef'Local TyName | TyRef'Foreign ModuleName TyName

newtype VarName = MkVarName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype VarName _

instance Prelude.Eq Kind where
  eq = (\x0 -> (\x1 -> case x0 of
                       Kind'Type  -> case x1 of
                                     Kind'Type  -> true
                                     Kind'Arrow x2 x3 -> false
                       Kind'Arrow x4 x5 -> case x1 of
                                           Kind'Type  -> false
                                           Kind'Arrow x6 x7 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) x4) x6))) ((Prelude.(==) x5) x7)) ) )
instance Prelude.Eq Ty where
  eq = (\x0 -> (\x1 -> case x0 of
                       Ty'App x2 x3 -> case x1 of
                                       Ty'App x4 x5 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) x2) x4))) ((Prelude.(==) x3) x5))
                                       Ty'Var x6 -> false
                                       Ty'Ref x7 -> false
                       Ty'Var x8 -> case x1 of
                                    Ty'App x9 x10 -> false
                                    Ty'Var x11 -> ((Prelude.(&&) true) ((Prelude.(==) x8) x11))
                                    Ty'Ref x12 -> false
                       Ty'Ref x13 -> case x1 of
                                     Ty'App x14 x15 -> false
                                     Ty'Var x16 -> false
                                     Ty'Ref x17 -> ((Prelude.(&&) true) ((Prelude.(==) x13) x17)) ) )
instance Prelude.Eq TyAbs where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).args) (Data.Newtype.unwrap x1).args))) ((Prelude.(==) (Data.Newtype.unwrap x0).body) (Data.Newtype.unwrap x1).body)) ) )
instance Prelude.Eq TyArg where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).kind) (Data.Newtype.unwrap x1).kind)) ) )
instance Prelude.Eq TyDef where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).abs) (Data.Newtype.unwrap x1).abs)) ) )
instance Prelude.Eq TyBody where
  eq = (\x0 -> (\x1 -> case x0 of
                       TyBody'Opaque  -> case x1 of
                                         TyBody'Opaque  -> true
                                         TyBody'Sum x2 -> false
                       TyBody'Sum x3 -> case x1 of
                                        TyBody'Opaque  -> false
                                        TyBody'Sum x4 -> ((Prelude.(&&) true) ((Prelude.(==) x3) x4)) ) )
instance Prelude.Eq Constructor where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).product) (Data.Newtype.unwrap x1).product)) ) )
instance Prelude.Eq Product where
  eq = (\x0 -> (\x1 -> let MkProduct x2 = x0 in let MkProduct x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq Record where
  eq = (\x0 -> (\x1 -> let MkRecord x2 = x0 in let MkRecord x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq Field where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).ty) (Data.Newtype.unwrap x1).ty)) ) )
instance Prelude.Eq TyRef where
  eq = (\x0 -> (\x1 -> case x0 of
                       TyRef'Local x2 -> case x1 of
                                         TyRef'Local x3 -> ((Prelude.(&&) true) ((Prelude.(==) x2) x3))
                                         TyRef'Foreign x4 x5 -> false
                       TyRef'Foreign x6 x7 -> case x1 of
                                              TyRef'Local x8 -> false
                                              TyRef'Foreign x9 x10 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) x6) x9))) ((Prelude.(==) x7) x10)) ) )
instance Prelude.Eq ClassDef where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).args) (Data.Newtype.unwrap x1).args))) ((Prelude.(==) (Data.Newtype.unwrap x0).supers) (Data.Newtype.unwrap x1).supers)) ) )
instance Prelude.Eq ClassConstraint where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).class) (Data.Newtype.unwrap x1).class))) ((Prelude.(==) (Data.Newtype.unwrap x0).args) (Data.Newtype.unwrap x1).args)) ) )
instance Prelude.Eq InstanceClause where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).head) (Data.Newtype.unwrap x1).head))) ((Prelude.(==) (Data.Newtype.unwrap x0).body) (Data.Newtype.unwrap x1).body)) ) )
instance Prelude.Eq Derive where
  eq = (\x0 -> (\x1 -> let MkDerive x2 = x0 in let MkDerive x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq Constraint where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).class) (Data.Newtype.unwrap x1).class))) ((Prelude.(==) (Data.Newtype.unwrap x0).args) (Data.Newtype.unwrap x1).args)) ) )
instance Prelude.Eq ClassRef where
  eq = (\x0 -> (\x1 -> case x0 of
                       ClassRef'Local x2 -> case x1 of
                                            ClassRef'Local x3 -> ((Prelude.(&&) true) ((Prelude.(==) x2) x3))
                                            ClassRef'Foreign x4 x5 -> false
                       ClassRef'Foreign x6 x7 -> case x1 of
                                                 ClassRef'Local x8 -> false
                                                 ClassRef'Foreign x9 x10 -> ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) x6) x9))) ((Prelude.(==) x7) x10)) ) )
instance Prelude.Eq Module where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) ((Prelude.(&&) ((Prelude.(&&) ((Prelude.(&&) ((Prelude.(&&) ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).name) (Data.Newtype.unwrap x1).name))) ((Prelude.(==) (Data.Newtype.unwrap x0).tyDefs) (Data.Newtype.unwrap x1).tyDefs))) ((Prelude.(==) (Data.Newtype.unwrap x0).classDefs) (Data.Newtype.unwrap x1).classDefs))) ((Prelude.(==) (Data.Newtype.unwrap x0).ruleImports) (Data.Newtype.unwrap x1).ruleImports))) ((Prelude.(==) (Data.Newtype.unwrap x0).instanceClauses) (Data.Newtype.unwrap x1).instanceClauses))) ((Prelude.(==) (Data.Newtype.unwrap x0).derives) (Data.Newtype.unwrap x1).derives)) ) )
instance Prelude.Eq CompilerInput where
  eq = (\x0 -> (\x1 -> ((Prelude.(&&) true) ((Prelude.(==) (Data.Newtype.unwrap x0).modules) (Data.Newtype.unwrap x1).modules)) ) )
instance Prelude.Eq TyName where
  eq = (\x0 -> (\x1 -> let MkTyName x2 = x0 in let MkTyName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq VarName where
  eq = (\x0 -> (\x1 -> let MkVarName x2 = x0 in let MkVarName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq ConstrName where
  eq = (\x0 -> (\x1 -> let MkConstrName x2 = x0 in let MkConstrName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq FieldName where
  eq = (\x0 -> (\x1 -> let MkFieldName x2 = x0 in let MkFieldName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq ModuleName where
  eq = (\x0 -> (\x1 -> let MkModuleName x2 = x0 in let MkModuleName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq ModuleNamePart where
  eq = (\x0 -> (\x1 -> let MkModuleNamePart x2 = x0 in let MkModuleNamePart x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )
instance Prelude.Eq ClassName where
  eq = (\x0 -> (\x1 -> let MkClassName x2 = x0 in let MkClassName x3 = x1 in ((Prelude.(&&) true) ((Prelude.(==) x2) x3)) ) )