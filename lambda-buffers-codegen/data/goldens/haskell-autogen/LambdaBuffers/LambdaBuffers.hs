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


data ClassConstraint = ClassConstraint { classConstraint'class :: ClassRef
                                       , classConstraint'args :: LambdaBuffers.Prelude.List TyArg} deriving Prelude.Show

data ClassDef = ClassDef { classDef'name :: ClassName
                         , classDef'args :: LambdaBuffers.Prelude.List TyArg
                         , classDef'supers :: LambdaBuffers.Prelude.List ClassConstraint} deriving Prelude.Show

newtype ClassName = ClassName LambdaBuffers.Prelude.Text deriving Prelude.Show

data ClassRef = ClassRef'Local ClassName
                 | ClassRef'Foreign ModuleName ClassName deriving Prelude.Show

newtype CompilerInput = CompilerInput { compilerInput'modules :: LambdaBuffers.Prelude.Map ModuleName
                                                                                           Module} deriving Prelude.Show

newtype ConstrName = ConstrName LambdaBuffers.Prelude.Text deriving Prelude.Show

data Constraint = Constraint { constraint'class :: ClassRef
                             , constraint'args :: LambdaBuffers.Prelude.List Ty} deriving Prelude.Show

data Constructor = Constructor { constructor'name :: ConstrName
                               , constructor'product :: Product} deriving Prelude.Show

newtype Derive = Derive Constraint deriving Prelude.Show

data Field = Field { field'name :: FieldName
                   , field'ty :: Ty} deriving Prelude.Show

newtype FieldName = FieldName LambdaBuffers.Prelude.Text deriving Prelude.Show

data InstanceClause = InstanceClause { instanceClause'head :: Constraint
                                     , instanceClause'body :: LambdaBuffers.Prelude.List Constraint} deriving Prelude.Show

data Kind = Kind'Type  | Kind'Arrow Kind Kind deriving Prelude.Show

data Module = Module { module'name :: ModuleName
                     , module'tyDefs :: LambdaBuffers.Prelude.Map TyName TyDef
                     , module'classDefs :: LambdaBuffers.Prelude.Map ClassName
                                                                     ClassDef
                     , module'ruleImports :: LambdaBuffers.Prelude.Set ModuleName
                     , module'instanceClauses :: LambdaBuffers.Prelude.List InstanceClause
                     , module'derives :: LambdaBuffers.Prelude.List Derive} deriving Prelude.Show

newtype ModuleName = ModuleName (LambdaBuffers.Prelude.List ModuleNamePart) deriving Prelude.Show

newtype ModuleNamePart = ModuleNamePart LambdaBuffers.Prelude.Text deriving Prelude.Show

newtype Product = Product (LambdaBuffers.Prelude.List Ty) deriving Prelude.Show

newtype Record = Record (LambdaBuffers.Prelude.Map FieldName
                                                   Field) deriving Prelude.Show

data Ty = Ty'App Ty Ty | Ty'Var VarName | Ty'Ref TyRef deriving Prelude.Show

data TyAbs = TyAbs { tyAbs'args :: LambdaBuffers.Prelude.List TyArg
                   , tyAbs'body :: TyBody} deriving Prelude.Show

data TyArg = TyArg { tyArg'name :: VarName
                   , tyArg'kind :: Kind} deriving Prelude.Show

data TyBody = TyBody'Opaque 
               | TyBody'Sum (LambdaBuffers.Prelude.Map ConstrName
                                                       Constructor) deriving Prelude.Show

data TyDef = TyDef { tyDef'name :: TyName
                   , tyDef'abs :: TyAbs} deriving Prelude.Show

newtype TyName = TyName LambdaBuffers.Prelude.Text deriving Prelude.Show

data TyRef = TyRef'Local TyName
              | TyRef'Foreign ModuleName TyName deriving Prelude.Show

newtype VarName = VarName LambdaBuffers.Prelude.Text deriving Prelude.Show


instance Prelude.Eq Kind where
  (==) = (\x0 -> (\x1 -> case x0 of
                           Kind'Type -> case x1 of
                                          Kind'Type -> Prelude.True
                                          Kind'Arrow x2 x3 -> Prelude.False
                           Kind'Arrow x4 x5 -> case x1 of
                                                 Kind'Type -> Prelude.False
                                                 Kind'Arrow x6 x7 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (x4) (x6))) ((Prelude.==) (x5) (x7)) ) )

instance Prelude.Eq Ty where
  (==) = (\x0 -> (\x1 -> case x0 of
                           Ty'App x2 x3 -> case x1 of
                                             Ty'App x4 x5 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x4))) ((Prelude.==) (x3) (x5))
                                             Ty'Var x6 -> Prelude.False
                                             Ty'Ref x7 -> Prelude.False
                           Ty'Var x8 -> case x1 of
                                          Ty'App x9 x10 -> Prelude.False
                                          Ty'Var x11 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x8) (x11))
                                          Ty'Ref x12 -> Prelude.False
                           Ty'Ref x13 -> case x1 of
                                           Ty'App x14 x15 -> Prelude.False
                                           Ty'Var x16 -> Prelude.False
                                           Ty'Ref x17 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x13) (x17)) ) )

instance Prelude.Eq TyAbs where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (tyAbs'args x0) (tyAbs'args x1))) ((Prelude.==) (tyAbs'body x0) (tyAbs'body x1)) ) )

instance Prelude.Eq TyArg where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (tyArg'name x0) (tyArg'name x1))) ((Prelude.==) (tyArg'kind x0) (tyArg'kind x1)) ) )

instance Prelude.Eq TyDef where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (tyDef'name x0) (tyDef'name x1))) ((Prelude.==) (tyDef'abs x0) (tyDef'abs x1)) ) )

instance Prelude.Eq TyBody where
  (==) = (\x0 -> (\x1 -> case x0 of
                           TyBody'Opaque -> case x1 of
                                              TyBody'Opaque -> Prelude.True
                                              TyBody'Sum x2 -> Prelude.False
                           TyBody'Sum x3 -> case x1 of
                                              TyBody'Opaque -> Prelude.False
                                              TyBody'Sum x4 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x3) (x4)) ) )

instance Prelude.Eq Constructor where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (constructor'name x0) (constructor'name x1))) ((Prelude.==) (constructor'product x0) (constructor'product x1)) ) )

instance Prelude.Eq Product where
  (==) = (\x0 -> (\x1 -> let Product x2 = x0 in let Product x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq Record where
  (==) = (\x0 -> (\x1 -> let Record x2 = x0 in let Record x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq Field where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (field'name x0) (field'name x1))) ((Prelude.==) (field'ty x0) (field'ty x1)) ) )

instance Prelude.Eq TyRef where
  (==) = (\x0 -> (\x1 -> case x0 of
                           TyRef'Local x2 -> case x1 of
                                               TyRef'Local x3 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3))
                                               TyRef'Foreign x4 x5 -> Prelude.False
                           TyRef'Foreign x6 x7 -> case x1 of
                                                    TyRef'Local x8 -> Prelude.False
                                                    TyRef'Foreign x9 x10 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (x6) (x9))) ((Prelude.==) (x7) (x10)) ) )

instance Prelude.Eq ClassDef where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (classDef'name x0) (classDef'name x1))) ((Prelude.==) (classDef'args x0) (classDef'args x1))) ((Prelude.==) (classDef'supers x0) (classDef'supers x1)) ) )

instance Prelude.Eq ClassConstraint where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (classConstraint'class x0) (classConstraint'class x1))) ((Prelude.==) (classConstraint'args x0) (classConstraint'args x1)) ) )

instance Prelude.Eq InstanceClause where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (instanceClause'head x0) (instanceClause'head x1))) ((Prelude.==) (instanceClause'body x0) (instanceClause'body x1)) ) )

instance Prelude.Eq Derive where
  (==) = (\x0 -> (\x1 -> let Derive x2 = x0 in let Derive x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq Constraint where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (constraint'class x0) (constraint'class x1))) ((Prelude.==) (constraint'args x0) (constraint'args x1)) ) )

instance Prelude.Eq ClassRef where
  (==) = (\x0 -> (\x1 -> case x0 of
                           ClassRef'Local x2 -> case x1 of
                                                  ClassRef'Local x3 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3))
                                                  ClassRef'Foreign x4 x5 -> Prelude.False
                           ClassRef'Foreign x6 x7 -> case x1 of
                                                       ClassRef'Local x8 -> Prelude.False
                                                       ClassRef'Foreign x9 x10 -> (Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (x6) (x9))) ((Prelude.==) (x7) (x10)) ) )

instance Prelude.Eq Module where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) ((Prelude.&&) ((Prelude.&&) ((Prelude.&&) ((Prelude.&&) ((Prelude.&&) (Prelude.True) ((Prelude.==) (module'name x0) (module'name x1))) ((Prelude.==) (module'tyDefs x0) (module'tyDefs x1))) ((Prelude.==) (module'classDefs x0) (module'classDefs x1))) ((Prelude.==) (module'ruleImports x0) (module'ruleImports x1))) ((Prelude.==) (module'instanceClauses x0) (module'instanceClauses x1))) ((Prelude.==) (module'derives x0) (module'derives x1)) ) )

instance Prelude.Eq CompilerInput where
  (==) = (\x0 -> (\x1 -> (Prelude.&&) (Prelude.True) ((Prelude.==) (compilerInput'modules x0) (compilerInput'modules x1)) ) )

instance Prelude.Eq TyName where
  (==) = (\x0 -> (\x1 -> let TyName x2 = x0 in let TyName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq VarName where
  (==) = (\x0 -> (\x1 -> let VarName x2 = x0 in let VarName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq ConstrName where
  (==) = (\x0 -> (\x1 -> let ConstrName x2 = x0 in let ConstrName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq FieldName where
  (==) = (\x0 -> (\x1 -> let FieldName x2 = x0 in let FieldName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq ModuleName where
  (==) = (\x0 -> (\x1 -> let ModuleName x2 = x0 in let ModuleName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq ModuleNamePart where
  (==) = (\x0 -> (\x1 -> let ModuleNamePart x2 = x0 in let ModuleNamePart x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )

instance Prelude.Eq ClassName where
  (==) = (\x0 -> (\x1 -> let ClassName x2 = x0 in let ClassName x3 = x1 in (Prelude.&&) (Prelude.True) ((Prelude.==) (x2) (x3)) ) )