module LambdaBuffers.Frontend.Utils (strip, TySym, ClassSym, Scope, TyScope, ClassScope, prettyTySym, prettyClassSym) where

import Control.Monad (void)
import Data.Map (Map)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (ClassName, ModuleAlias, ModuleName, TyName)
import Prettyprinter (Doc, Pretty (pretty), dot)

type TySym = (Maybe (ModuleAlias ()), TyName ())
type ClassSym = (Maybe (ModuleAlias ()), ClassName ())

type TyScope = Map TySym (ModuleName ())
type ClassScope = Map ClassSym (ModuleName ())

type Scope = (TyScope, ClassScope)

strip :: Functor f => f a -> f ()
strip = void

prettyTySym :: TySym -> Doc ann
prettyTySym (Nothing, tn) = pretty tn
prettyTySym (Just al, tn) = pretty al <> dot <> pretty tn

prettyClassSym :: ClassSym -> Doc ann
prettyClassSym (Nothing, cn) = pretty cn
prettyClassSym (Just al, cn) = pretty al <> dot <> pretty cn
