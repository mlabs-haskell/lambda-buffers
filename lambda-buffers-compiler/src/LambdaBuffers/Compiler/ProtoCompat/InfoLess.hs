{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Generics.SOP (All2, Generic (Code, from, to), Proxy (..), hcmap, mapII)
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
class Eq a => SILId a where
  silId :: a -> a
  default silId :: (Generic a, All2 SILId (Code a)) => a -> a
  silId = gsilId

-- | Make InfoLess Datatype.
mkInfoLess :: SILId a => a -> InfoLess a
mkInfoLess = InfoLess . silId

gsilId :: (Generic a, All2 SILId (Code a)) => a -> a
gsilId = to . hcmap (Proxy :: Proxy SILId) (mapII silId) . from

-- | Work with Info Less.
withInfoLess :: SILId a => InfoLess a -> (a -> b) -> b
withInfoLess (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

-- | Work with Info Less - functor way.
withInfoLessF :: forall f {a} {b}. SILId a => InfoLess a -> (a -> f b) -> f b
withInfoLessF (InfoLess a) f = f . unsafeInfoLess . mkInfoLess $ a

instance SILId a => SILId [a] where
  silId = fmap silId

instance SILId Int where
  silId = id

instance SILId Text where
  silId = id

instance (Ord k, SILId k, SILId v) => SILId (M.Map k v) where
  silId = M.fromList . fmap (bimap silId silId) . M.toList

instance (Ord a, SILId a) => SILId (S.Set a) where
  silId = S.fromList . fmap silId . S.toList

instance SILId SourceInfo where
  silId = const def

instance Default SourceInfo where
  def = defSourceInfo

instance SILId SourcePosition
instance SILId LBName
instance SILId TyName
instance SILId ConstrName
instance SILId ModuleName
instance SILId ModuleNamePart
instance SILId VarName
instance SILId FieldName
instance SILId ClassName
instance SILId Kind
instance SILId KindType
instance SILId KindRefType
instance SILId TyVar
instance SILId Ty
instance SILId TyApp
instance SILId ForeignRef
instance SILId LocalRef
instance SILId TyRef
instance SILId TyDef
instance SILId TyAbs
instance SILId TyArg
instance SILId TyBody
instance SILId Constructor
instance SILId Sum
instance SILId Field
instance SILId Record
instance SILId Tuple
instance SILId Product
instance SILId ForeignClassRef
instance SILId LocalClassRef
instance SILId TyClassRef
instance SILId ClassDef
instance SILId InstanceClause
instance SILId Constraint
instance SILId Module
instance SILId InferenceErr
instance SILId KindCheckErr
instance SILId CompilerInput
instance SILId KindCheckError
instance SILId CompilerError
instance SILId CompilerResult
