module LambdaBuffers.Compiler.ProtoCompat.Eval (Ty (..), fromTy, eval) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (MonadReader (ask, local))
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.ProtoCompat qualified as PC

data Ty
  = TyApp Ty [Ty] PC.TyApp
  | TyAbs (OMap (PC.InfoLess PC.VarName) PC.TyArg) Ty PC.TyAbs
  | TyRef PC.TyRef
  | TyVar PC.TyVar
  | TyOpaque PC.SourceInfo
  | TySum (OMap (PC.InfoLess PC.ConstrName) Ty) PC.Sum
  | TyTuple [Ty] PC.Product
  | TyRecord (OMap (PC.InfoLess PC.FieldName) Ty) PC.Product

fromTy :: PC.Ty -> Ty
fromTy (PC.TyVarI tv) = TyVar tv
fromTy (PC.TyRefI tr) = TyRef tr
fromTy (PC.TyAppI ta@(PC.TyApp tf as _)) = TyApp (fromTy tf) (fromTy <$> as) ta

fromTyAbs :: PC.TyAbs -> Ty
fromTyAbs tabs@(PC.TyAbs args body _si) = TyAbs args (fromTyBody body) tabs

fromTyBody :: PC.TyBody -> Ty
fromTyBody (PC.OpaqueI si) = TyOpaque si
fromTyBody (PC.SumI s) = TySum (fromTyCtor <$> s ^. #constructors) s
  where
    fromTyCtor (PC.Constructor _cn p) = fromTyProd p

fromTyProd :: PC.Product -> Ty
fromTyProd p@(PC.TupleI (PC.Tuple fields _)) = TyTuple (fromTy <$> fields) p
fromTyProd p@(PC.RecordI (PC.Record fields _)) = TyRecord (fromTyField <$> fields) p
  where
    fromTyField (PC.Field _fn ty) = fromTy ty

type Context = (PC.ModuleName, Map PC.QTyName PC.TyDef, Map (PC.InfoLess PC.VarName) Ty)

type MonadEval m = (MonadError Text m, MonadReader Context m)

eval :: MonadEval m => Ty -> m Ty
eval (TyRef tr) = resolveTyRef tr >>= eval
eval (TyApp (TyAbs args' body _) tys _) =
  let actx' = Map.fromList $ zip (PC.mkInfoLess . view #argName <$> toList args') tys
   in local (\(mn, tds, actx) -> (mn, tds, Map.union actx' actx)) (subst body)
eval t = return t

resolveTyRef :: MonadEval m => PC.TyRef -> m Ty
resolveTyRef (PC.LocalI tr@(PC.LocalRef tn _si)) = do
  (mn, tds, _args) <- ask
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tn) tds of
    Nothing -> throwError ("TODO(bladyjoker): UnboundTyRef " <> (Text.pack . show $ tr))
    Just td -> return $ fromTyAbs (td ^. #tyAbs)
resolveTyRef (PC.ForeignI tr@(PC.ForeignRef tn mn _si)) = do
  (_mn, tds, _args) <- ask
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tn) tds of
    Nothing -> throwError ("TODO(bladyjoker): UnboundTyRef " <> (Text.pack . show $ tr))
    Just td -> return $ fromTyAbs (td ^. #tyAbs)

resolveTyVar :: MonadEval m => PC.TyVar -> m Ty
resolveTyVar tv = do
  (_mn, _td, args) <- ask
  case Map.lookup (PC.mkInfoLess $ tv ^. #varName) args of
    Nothing -> throwError $ "TODO(bladyjoker): UnboundTyVar " <> (Text.pack . show $ tv)
    Just ty -> return ty

subst :: MonadEval m => Ty -> m Ty
subst (TyVar tv) = resolveTyVar tv
subst (TyAbs args' body _) = do
  (mn, tds, args) <- ask
  let x = Map.difference args (OMap.toMap args')
  local (const (mn, tds, x)) (subst body)
subst (TyApp f args t) = TyApp <$> subst f <*> (subst `traverse` args) <*> pure t
subst (TySum ctors s) = TySum <$> (subst `traverse` ctors) <*> pure s
subst (TyTuple fields t) = TyTuple <$> (subst `traverse` fields) <*> pure t
subst (TyRecord fields r) = TyRecord <$> (subst `traverse` fields) <*> pure r
subst t = return t
