module LambdaBuffers.Codegen.Haskell.Syntax (CabalPackageName (..), ModuleName (..), TyName (..), ClassName (..), FunctionName (..)) where

import Data.Text (Text)

newtype CabalPackageName = MkCabalPackageName Text deriving stock (Eq, Ord, Show)
newtype ModuleName = MkModuleName Text deriving stock (Eq, Ord, Show)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show)
newtype ClassName = MkClassName Text deriving stock (Eq, Ord, Show)
newtype FunctionName = MkFunctionName Text deriving stock (Eq, Ord, Show)
