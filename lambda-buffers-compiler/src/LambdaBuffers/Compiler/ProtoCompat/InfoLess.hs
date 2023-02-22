{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Compiler.ProtoCompat.InfoLess (
  InfoLess,
  withInfoLess,
  withInfoLessF,
  mkInfoLess,
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Default (Default (def))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Generics.SOP (All2, Generic (Code, from, to), Proxy (Proxy), hcmap, mapII)
import LambdaBuffers.Compiler.ProtoCompat.Types (
  ClassDef,
  ClassName,
  CompilerError,
  CompilerInput,
  CompilerResult,
  ConstrName,
  Constraint,
  Constructor,
  Field,
  FieldName,
  ForeignClassRef,
  ForeignRef,
  InferenceErr,
  InstanceClause,
  Kind,
  KindCheckErr,
  KindCheckError,
  KindRefType,
  KindType,
  LBName,
  LocalClassRef,
  LocalRef,
  Module,
  ModuleName,
  ModuleNamePart,
  Product,
  Record,
  SourceInfo,
  SourcePosition,
  Sum,
  Tuple,
  Ty,
  TyAbs,
  TyApp,
  TyArg,
  TyBody,
  TyClassRef,
  TyDef,
  TyName,
  TyRef,
  TyVar,
  VarName,
  defSourceInfo,
 )

-- | InfoLess newtype. Constructor is not exported to not allow the construction of types with the Info. InfoLess a can only be constructed via its class instance and deconstructed using the exported function.
newtype InfoLess a = InfoLess {unsafeInfoLess :: a}
  deriving stock (Show, Eq, Ord)
  deriving stock (Functor, Traversable, Foldable)

{- | SourceInfo Less ID.
 A TypeClass that provides id for types with SourceInfo - where SI is defaulted - therefore ignored. Not exported for obvious unsafe reasons.
-}
class Eq a => InfoLessC a where
  silId :: a -> a
  default silId :: (Generic a, All2 InfoLessC (Code a)) => a -> a
  silId = gsilId

-- | Make InfoLess Datatype.
mkInfoLess :: InfoLessC a => a -> InfoLess a
mkInfoLess = InfoLess . silId

gsilId :: (Generic a, All2 InfoLessC (Code a)) => a -> a
gsilId = to . hcmap (Proxy :: Proxy InfoLessC) (mapII silId) . from

-- | Work with Info Less.
withInfoLess :: InfoLessC a => InfoLess a -> (a -> b) -> b
withInfoLess (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

-- | Work with Info Less - functor way.
withInfoLessF :: forall f {a} {b}. InfoLessC a => InfoLess a -> (a -> f b) -> f b
withInfoLessF (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

instance InfoLessC a => InfoLessC [a] where
  silId = fmap silId

instance InfoLessC Int where
  silId = id

instance InfoLessC Text where
  silId = id

instance (Ord k, InfoLessC k, InfoLessC v) => InfoLessC (M.Map k v) where
  silId = M.fromList . fmap (bimap silId silId) . M.toList

instance (Ord a, InfoLessC a) => InfoLessC (S.Set a) where
  silId = S.fromList . fmap silId . S.toList

instance InfoLessC SourceInfo where
  silId = const def

instance Default SourceInfo where
  def = defSourceInfo

instance InfoLessC SourcePosition
instance InfoLessC LBName
instance InfoLessC TyName
instance InfoLessC ConstrName
instance InfoLessC ModuleName
instance InfoLessC ModuleNamePart
instance InfoLessC VarName
instance InfoLessC FieldName
instance InfoLessC ClassName
instance InfoLessC Kind
instance InfoLessC KindType
instance InfoLessC KindRefType
instance InfoLessC TyVar
instance InfoLessC Ty
instance InfoLessC TyApp
instance InfoLessC ForeignRef
instance InfoLessC LocalRef
instance InfoLessC TyRef
instance InfoLessC TyDef
instance InfoLessC TyAbs
instance InfoLessC TyArg
instance InfoLessC TyBody
instance InfoLessC Constructor
instance InfoLessC Sum
instance InfoLessC Field
instance InfoLessC Record
instance InfoLessC Tuple
instance InfoLessC Product
instance InfoLessC ForeignClassRef
instance InfoLessC LocalClassRef
instance InfoLessC TyClassRef
instance InfoLessC ClassDef
instance InfoLessC InstanceClause
instance InfoLessC Constraint
instance InfoLessC Module
instance InfoLessC InferenceErr
instance InfoLessC KindCheckErr
instance InfoLessC CompilerInput
instance InfoLessC KindCheckError
instance InfoLessC CompilerError
instance InfoLessC CompilerResult
