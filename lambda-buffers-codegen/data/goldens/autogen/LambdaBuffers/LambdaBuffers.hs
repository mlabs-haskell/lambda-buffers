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
                                   , Foo
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
import qualified PlutusTx
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
data Foo a b c = Foo'Bar a b c | Foo'Baz a b c | Foo'Bax a b c
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

instance (Prelude.Eq a,Prelude.Eq b,Prelude.Eq c) => Prelude.Eq (Foo a
                                                                     b
                                                                     c) where
  (==) = (\x0 -> (\x1 -> case x0 of
                         Foo'Bar x2 x3 x4 -> case x1 of
                                             Foo'Bar x5 x6 x7 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x2) x5))) (((Prelude.==) x3) x6))) (((Prelude.==) x4) x7))
                                             Foo'Baz x8 x9 x10 -> Prelude.False
                                             Foo'Bax x11 x12 x13 -> Prelude.False
                         Foo'Baz x14 x15 x16 -> case x1 of
                                                Foo'Bar x17 x18 x19 -> Prelude.False
                                                Foo'Baz x20 x21 x22 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x14) x20))) (((Prelude.==) x15) x21))) (((Prelude.==) x16) x22))
                                                Foo'Bax x23 x24 x25 -> Prelude.False
                         Foo'Bax x26 x27 x28 -> case x1 of
                                                Foo'Bar x29 x30 x31 -> Prelude.False
                                                Foo'Baz x32 x33 x34 -> Prelude.False
                                                Foo'Bax x35 x36 x37 -> (((Prelude.&&) (((Prelude.&&) (((Prelude.&&) Prelude.True) (((Prelude.==) x26) x35))) (((Prelude.==) x27) x36))) (((Prelude.==) x28) x37)) ) )
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
instance (PlutusTx.ToData a
         ,PlutusTx.ToData b
         ,PlutusTx.ToData c) => PlutusTx.ToData (Foo a b c) where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> case x0 of
                                      Foo'Bar x1 x2 x3 -> ((PlutusTx.Constr 0) [(PlutusTx.toData x1)
                                                                               ,(PlutusTx.toData x2)
                                                                               ,(PlutusTx.toData x3)])
                                      Foo'Baz x4 x5 x6 -> ((PlutusTx.Constr 1) [(PlutusTx.toData x4)
                                                                               ,(PlutusTx.toData x5)
                                                                               ,(PlutusTx.toData x6)])
                                      Foo'Bax x7 x8 x9 -> ((PlutusTx.Constr 2) [(PlutusTx.toData x7)
                                                                               ,(PlutusTx.toData x8)
                                                                               ,(PlutusTx.toData x9)]) )
instance PlutusTx.ToData Module where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> (PlutusTx.List [(PlutusTx.toData (module'name x0))
                                                     ,(PlutusTx.toData (module'tyDefs x0))
                                                     ,(PlutusTx.toData (module'classDefs x0))
                                                     ,(PlutusTx.toData (module'ruleImports x0))
                                                     ,(PlutusTx.toData (module'instanceClauses x0))
                                                     ,(PlutusTx.toData (module'derives x0))]) )
instance PlutusTx.ToData TyRef where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> case x0 of
                                      TyRef'Local x1 -> ((PlutusTx.Constr 0) [(PlutusTx.toData x1)])
                                      TyRef'Foreign x2 x3 -> ((PlutusTx.Constr 1) [(PlutusTx.toData x2)
                                                                                  ,(PlutusTx.toData x3)]) )
instance PlutusTx.ToData ModuleName where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkModuleName x1 = x0 in (PlutusTx.toData x1) )
instance PlutusTx.ToData ModuleNamePart where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkModuleNamePart x1 = x0 in (PlutusTx.toData x1) )
instance PlutusTx.ToData TyName where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkTyName x1 = x0 in (PlutusTx.toData x1) )
instance PlutusTx.ToData VarName where
  toBuiltinData = (Prelude..) PlutusTx.dataToBuiltinData
                              (\x0 -> let MkVarName x1 = x0 in (PlutusTx.toData x1) )