module LambdaBuffers.Compiler.LamTy.Types (Ty (..), fromTy, fromTyAbs) where

import Control.Lens ((^.))
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered (OMap)
import LambdaBuffers.Compiler.ProtoCompat qualified as PC

data Ty
  = TyApp Ty [Ty] (Maybe PC.TyApp)
  | TyAbs (OMap (PC.InfoLess PC.VarName) PC.TyArg) Ty PC.TyAbs
  | TyRef PC.TyRef
  | TyVar PC.TyVar
  | TyOpaque PC.SourceInfo
  | TySum (OMap (PC.InfoLess PC.ConstrName) Ty) PC.Sum
  | TyProduct [Ty] PC.Product
  | TyRecord (OMap (PC.InfoLess PC.FieldName) Ty) PC.Record
  deriving stock (Eq, Ord)

fromTy :: PC.Ty -> Ty
fromTy (PC.TyVarI tv) = TyVar tv
fromTy (PC.TyRefI tr) = TyRef tr
fromTy (PC.TyAppI ta@(PC.TyApp tf as _)) = TyApp (fromTy tf) (fromTy <$> as) (Just ta)

fromTyAbs :: PC.TyAbs -> Ty
fromTyAbs tabs@(PC.TyAbs args body _si) = TyAbs args (fromTyBody args body) tabs

fromTyBody :: OMap (PC.InfoLess PC.VarName) PC.TyArg -> PC.TyBody -> Ty
fromTyBody args (PC.OpaqueI si) = TyApp (TyOpaque si) (fromTyArg <$> toList args) Nothing
  where
    fromTyArg :: PC.TyArg -> Ty
    fromTyArg arg = TyVar $ PC.TyVar (arg ^. #argName)
fromTyBody _ (PC.SumI s) = TySum (fromTyCtor <$> s ^. #constructors) s
  where
    fromTyCtor (PC.Constructor _cn p) = fromTyProd p
fromTyBody _ (PC.ProductI p) = fromTyProd p
fromTyBody _ (PC.RecordI r) = fromTyRecord r

fromTyProd :: PC.Product -> Ty
fromTyProd p@(PC.Product fields _) = TyProduct (fromTy <$> fields) p

fromTyRecord :: PC.Record -> Ty
fromTyRecord p@(PC.Record fields _) = TyRecord (fromTyField <$> fields) p
  where
    fromTyField (PC.Field _fn ty) = fromTy ty
