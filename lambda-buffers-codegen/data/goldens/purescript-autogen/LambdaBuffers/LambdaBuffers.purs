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
import Data.Generic.Rep as Data.Generic.Rep
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Prelude as Prelude


newtype ClassConstraint = ClassConstraint { class :: ClassRef
                                          , args :: LambdaBuffers.Prelude.List TyArg}
derive instance Data.Newtype.Newtype ClassConstraint _
derive instance Data.Generic.Rep.Generic ClassConstraint _
instance Data.Show.Show ClassConstraint where
  show = Data.Show.Generic.genericShow

newtype ClassDef = ClassDef { name :: ClassName
                            , args :: LambdaBuffers.Prelude.List TyArg
                            , supers :: LambdaBuffers.Prelude.List ClassConstraint}
derive instance Data.Newtype.Newtype ClassDef _
derive instance Data.Generic.Rep.Generic ClassDef _
instance Data.Show.Show ClassDef where
  show = Data.Show.Generic.genericShow

newtype ClassName = ClassName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ClassName _
derive instance Data.Generic.Rep.Generic ClassName _
instance Data.Show.Show ClassName where
  show = Data.Show.Generic.genericShow

data ClassRef = ClassRef'Local ClassName | ClassRef'Foreign ModuleName ClassName
derive instance Data.Generic.Rep.Generic ClassRef _
instance Data.Show.Show ClassRef where
  show = Data.Show.Generic.genericShow

newtype CompilerInput = CompilerInput { modules :: LambdaBuffers.Prelude.Map ModuleName
                                                                             Module}
derive instance Data.Newtype.Newtype CompilerInput _
derive instance Data.Generic.Rep.Generic CompilerInput _
instance Data.Show.Show CompilerInput where
  show = Data.Show.Generic.genericShow

newtype ConstrName = ConstrName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ConstrName _
derive instance Data.Generic.Rep.Generic ConstrName _
instance Data.Show.Show ConstrName where
  show = Data.Show.Generic.genericShow

newtype Constraint = Constraint { class :: ClassRef
                                , args :: LambdaBuffers.Prelude.List Ty}
derive instance Data.Newtype.Newtype Constraint _
derive instance Data.Generic.Rep.Generic Constraint _
instance Data.Show.Show Constraint where
  show = Data.Show.Generic.genericShow

newtype Constructor = Constructor { name :: ConstrName, product :: Product}
derive instance Data.Newtype.Newtype Constructor _
derive instance Data.Generic.Rep.Generic Constructor _
instance Data.Show.Show Constructor where
  show = Data.Show.Generic.genericShow

newtype Derive = Derive Constraint
derive instance Data.Newtype.Newtype Derive _
derive instance Data.Generic.Rep.Generic Derive _
instance Data.Show.Show Derive where
  show = Data.Show.Generic.genericShow

newtype Field = Field { name :: FieldName, ty :: Ty}
derive instance Data.Newtype.Newtype Field _
derive instance Data.Generic.Rep.Generic Field _
instance Data.Show.Show Field where
  show = Data.Show.Generic.genericShow

newtype FieldName = FieldName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype FieldName _
derive instance Data.Generic.Rep.Generic FieldName _
instance Data.Show.Show FieldName where
  show = Data.Show.Generic.genericShow

newtype InstanceClause = InstanceClause { head :: Constraint
                                        , body :: LambdaBuffers.Prelude.List Constraint}
derive instance Data.Newtype.Newtype InstanceClause _
derive instance Data.Generic.Rep.Generic InstanceClause _
instance Data.Show.Show InstanceClause where
  show = Data.Show.Generic.genericShow

data Kind = Kind'Type  | Kind'Arrow Kind Kind
derive instance Data.Generic.Rep.Generic Kind _
instance Data.Show.Show Kind where
  show = Data.Show.Generic.genericShow

newtype Module = Module { name :: ModuleName
                        , tyDefs :: LambdaBuffers.Prelude.Map TyName TyDef
                        , classDefs :: LambdaBuffers.Prelude.Map ClassName
                                                                 ClassDef
                        , ruleImports :: LambdaBuffers.Prelude.Set ModuleName
                        , instanceClauses :: LambdaBuffers.Prelude.List InstanceClause
                        , derives :: LambdaBuffers.Prelude.List Derive}
derive instance Data.Newtype.Newtype Module _
derive instance Data.Generic.Rep.Generic Module _
instance Data.Show.Show Module where
  show = Data.Show.Generic.genericShow

newtype ModuleName = ModuleName (LambdaBuffers.Prelude.List ModuleNamePart)
derive instance Data.Newtype.Newtype ModuleName _
derive instance Data.Generic.Rep.Generic ModuleName _
instance Data.Show.Show ModuleName where
  show = Data.Show.Generic.genericShow

newtype ModuleNamePart = ModuleNamePart LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype ModuleNamePart _
derive instance Data.Generic.Rep.Generic ModuleNamePart _
instance Data.Show.Show ModuleNamePart where
  show = Data.Show.Generic.genericShow

newtype Product = Product (LambdaBuffers.Prelude.List Ty)
derive instance Data.Newtype.Newtype Product _
derive instance Data.Generic.Rep.Generic Product _
instance Data.Show.Show Product where
  show = Data.Show.Generic.genericShow

newtype Record = Record (LambdaBuffers.Prelude.Map FieldName Field)
derive instance Data.Newtype.Newtype Record _
derive instance Data.Generic.Rep.Generic Record _
instance Data.Show.Show Record where
  show = Data.Show.Generic.genericShow

data Ty = Ty'App Ty Ty | Ty'Var VarName | Ty'Ref TyRef
derive instance Data.Generic.Rep.Generic Ty _
instance Data.Show.Show Ty where
  show = Data.Show.Generic.genericShow

newtype TyAbs = TyAbs { args :: LambdaBuffers.Prelude.List TyArg
                      , body :: TyBody}
derive instance Data.Newtype.Newtype TyAbs _
derive instance Data.Generic.Rep.Generic TyAbs _
instance Data.Show.Show TyAbs where
  show = Data.Show.Generic.genericShow

newtype TyArg = TyArg { name :: VarName, kind :: Kind}
derive instance Data.Newtype.Newtype TyArg _
derive instance Data.Generic.Rep.Generic TyArg _
instance Data.Show.Show TyArg where
  show = Data.Show.Generic.genericShow

data TyBody = TyBody'Opaque 
               | TyBody'Sum (LambdaBuffers.Prelude.Map ConstrName Constructor)
derive instance Data.Generic.Rep.Generic TyBody _
instance Data.Show.Show TyBody where
  show = Data.Show.Generic.genericShow

newtype TyDef = TyDef { name :: TyName, abs :: TyAbs}
derive instance Data.Newtype.Newtype TyDef _
derive instance Data.Generic.Rep.Generic TyDef _
instance Data.Show.Show TyDef where
  show = Data.Show.Generic.genericShow

newtype TyName = TyName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype TyName _
derive instance Data.Generic.Rep.Generic TyName _
instance Data.Show.Show TyName where
  show = Data.Show.Generic.genericShow

data TyRef = TyRef'Local TyName | TyRef'Foreign ModuleName TyName
derive instance Data.Generic.Rep.Generic TyRef _
instance Data.Show.Show TyRef where
  show = Data.Show.Generic.genericShow

newtype VarName = VarName LambdaBuffers.Prelude.Text
derive instance Data.Newtype.Newtype VarName _
derive instance Data.Generic.Rep.Generic VarName _
instance Data.Show.Show VarName where
  show = Data.Show.Generic.genericShow


instance Prelude.Eq Kind where
  eq = (\x0 -> (\x1 -> case x0 of
                       Kind'Type -> case x1 of
                                    Kind'Type -> true
                                    Kind'Arrow x2 x3 -> false
                       Kind'Arrow x4 x5 -> case x1 of
                                           Kind'Type -> false
                                           Kind'Arrow x6 x7 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) (x4) (x6))) (Prelude.(==) (x5) (x7)) ) )

instance Prelude.Eq Ty where
  eq = (\x0 -> (\x1 -> case x0 of
                       Ty'App x2 x3 -> case x1 of
                                       Ty'App x4 x5 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) (x2) (x4))) (Prelude.(==) (x3) (x5))
                                       Ty'Var x6 -> false
                                       Ty'Ref x7 -> false
                       Ty'Var x8 -> case x1 of
                                    Ty'App x9 x10 -> false
                                    Ty'Var x11 -> Prelude.(&&) (true) (Prelude.(==) (x8) (x11))
                                    Ty'Ref x12 -> false
                       Ty'Ref x13 -> case x1 of
                                     Ty'App x14 x15 -> false
                                     Ty'Var x16 -> false
                                     Ty'Ref x17 -> Prelude.(&&) (true) (Prelude.(==) (x13) (x17)) ) )

instance Prelude.Eq TyAbs where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).args) ((Data.Newtype.unwrap x1).args))) (Prelude.(==) ((Data.Newtype.unwrap x0).body) ((Data.Newtype.unwrap x1).body)) ) )

instance Prelude.Eq TyArg where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).kind) ((Data.Newtype.unwrap x1).kind)) ) )

instance Prelude.Eq TyDef where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).abs) ((Data.Newtype.unwrap x1).abs)) ) )

instance Prelude.Eq TyBody where
  eq = (\x0 -> (\x1 -> case x0 of
                       TyBody'Opaque -> case x1 of
                                        TyBody'Opaque -> true
                                        TyBody'Sum x2 -> false
                       TyBody'Sum x3 -> case x1 of
                                        TyBody'Opaque -> false
                                        TyBody'Sum x4 -> Prelude.(&&) (true) (Prelude.(==) (x3) (x4)) ) )

instance Prelude.Eq Constructor where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).product) ((Data.Newtype.unwrap x1).product)) ) )

instance Prelude.Eq Product where
  eq = (\x0 -> (\x1 -> let Product x2 = x0 in let Product x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq Record where
  eq = (\x0 -> (\x1 -> let Record x2 = x0 in let Record x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq Field where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).ty) ((Data.Newtype.unwrap x1).ty)) ) )

instance Prelude.Eq TyRef where
  eq = (\x0 -> (\x1 -> case x0 of
                       TyRef'Local x2 -> case x1 of
                                         TyRef'Local x3 -> Prelude.(&&) (true) (Prelude.(==) (x2) (x3))
                                         TyRef'Foreign x4 x5 -> false
                       TyRef'Foreign x6 x7 -> case x1 of
                                              TyRef'Local x8 -> false
                                              TyRef'Foreign x9 x10 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) (x6) (x9))) (Prelude.(==) (x7) (x10)) ) )

instance Prelude.Eq ClassDef where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).args) ((Data.Newtype.unwrap x1).args))) (Prelude.(==) ((Data.Newtype.unwrap x0).supers) ((Data.Newtype.unwrap x1).supers)) ) )

instance Prelude.Eq ClassConstraint where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).class) ((Data.Newtype.unwrap x1).class))) (Prelude.(==) ((Data.Newtype.unwrap x0).args) ((Data.Newtype.unwrap x1).args)) ) )

instance Prelude.Eq InstanceClause where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).head) ((Data.Newtype.unwrap x1).head))) (Prelude.(==) ((Data.Newtype.unwrap x0).body) ((Data.Newtype.unwrap x1).body)) ) )

instance Prelude.Eq Derive where
  eq = (\x0 -> (\x1 -> let Derive x2 = x0 in let Derive x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq Constraint where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).class) ((Data.Newtype.unwrap x1).class))) (Prelude.(==) ((Data.Newtype.unwrap x0).args) ((Data.Newtype.unwrap x1).args)) ) )

instance Prelude.Eq ClassRef where
  eq = (\x0 -> (\x1 -> case x0 of
                       ClassRef'Local x2 -> case x1 of
                                            ClassRef'Local x3 -> Prelude.(&&) (true) (Prelude.(==) (x2) (x3))
                                            ClassRef'Foreign x4 x5 -> false
                       ClassRef'Foreign x6 x7 -> case x1 of
                                                 ClassRef'Local x8 -> false
                                                 ClassRef'Foreign x9 x10 -> Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) (x6) (x9))) (Prelude.(==) (x7) (x10)) ) )

instance Prelude.Eq Module where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).name) ((Data.Newtype.unwrap x1).name))) (Prelude.(==) ((Data.Newtype.unwrap x0).tyDefs) ((Data.Newtype.unwrap x1).tyDefs))) (Prelude.(==) ((Data.Newtype.unwrap x0).classDefs) ((Data.Newtype.unwrap x1).classDefs))) (Prelude.(==) ((Data.Newtype.unwrap x0).ruleImports) ((Data.Newtype.unwrap x1).ruleImports))) (Prelude.(==) ((Data.Newtype.unwrap x0).instanceClauses) ((Data.Newtype.unwrap x1).instanceClauses))) (Prelude.(==) ((Data.Newtype.unwrap x0).derives) ((Data.Newtype.unwrap x1).derives)) ) )

instance Prelude.Eq CompilerInput where
  eq = (\x0 -> (\x1 -> Prelude.(&&) (true) (Prelude.(==) ((Data.Newtype.unwrap x0).modules) ((Data.Newtype.unwrap x1).modules)) ) )

instance Prelude.Eq TyName where
  eq = (\x0 -> (\x1 -> let TyName x2 = x0 in let TyName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq VarName where
  eq = (\x0 -> (\x1 -> let VarName x2 = x0 in let VarName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq ConstrName where
  eq = (\x0 -> (\x1 -> let ConstrName x2 = x0 in let ConstrName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq FieldName where
  eq = (\x0 -> (\x1 -> let FieldName x2 = x0 in let FieldName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq ModuleName where
  eq = (\x0 -> (\x1 -> let ModuleName x2 = x0 in let ModuleName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq ModuleNamePart where
  eq = (\x0 -> (\x1 -> let ModuleNamePart x2 = x0 in let ModuleNamePart x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )

instance Prelude.Eq ClassName where
  eq = (\x0 -> (\x1 -> let ClassName x2 = x0 in let ClassName x3 = x1 in Prelude.(&&) (true) (Prelude.(==) (x2) (x3)) ) )