module LambdaBuffers.Compiler.ProtoCompat.Eval (Ty (..), fromTy, eval) where

import Control.Lens (makeLenses, view, (%~), (&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data Ty
  = TyApp Ty [Ty] PC.TyApp
  | TyAbs (OMap (PC.InfoLess PC.VarName) PC.TyArg) Ty PC.TyAbs
  | TyRef PC.TyRef
  | TyVar PC.TyVar
  | TyOpaque PC.SourceInfo
  | TySum (OMap (PC.InfoLess PC.ConstrName) Ty) PC.Sum
  | TyTuple [Ty] PC.Product
  | TyRecord (OMap (PC.InfoLess PC.FieldName) Ty) PC.Product
  deriving stock (Eq, Ord, Show)

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

data Context = MkContext
  { _ctxModuleName :: PC.ModuleName
  , _ctxTyDefs :: Map PC.QTyName PC.TyDef
  , _ctxTyArgs :: Map (PC.InfoLess PC.VarName) Ty
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkContext

type MonadEval m = (MonadError P.CompilerError m, MonadReader Context m)

eval :: MonadEval m => Ty -> m Ty
eval (TyRef tr) = resolveTyRef tr >>= eval
eval (TyApp (TyAbs args' body _) tys _) =
  let actx' = Map.fromList $ zip (PC.mkInfoLess . view #argName <$> toList args') tys
   in local (\ctx -> ctx & ctxTyArgs %~ Map.union actx') (subst body)
eval t = return t

resolveTyRef :: MonadEval m => PC.TyRef -> m Ty
resolveTyRef tr@(PC.LocalI (PC.LocalRef tn _si)) = do
  mn <- asks (view ctxModuleName)
  tds <- asks (view ctxTyDefs)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tn) tds of
    Nothing ->
      throwError $
        defMessage
          & P.kindCheckErrors -- FIXME(bladyjoker): Parametrize error messages on 'stage' (KindChecking being one of them).
            .~ [ defMessage
                  & P.unboundTyRefError . P.moduleName .~ PC.toProto mn
                  & P.unboundTyRefError . P.tyRef .~ PC.toProto tr
               ]
    Just td -> return $ fromTyAbs (td ^. #tyAbs)
resolveTyRef tr@(PC.ForeignI (PC.ForeignRef tn mn _si)) = do
  tds <- asks (view ctxTyDefs)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tn) tds of
    Nothing ->
      throwError $
        defMessage
          & P.kindCheckErrors -- FIXME(bladyjoker): Parametrize error messages on 'stage' (KindChecking being one of them).
            .~ [ defMessage
                  & P.unboundTyRefError . P.moduleName .~ PC.toProto mn
                  & P.unboundTyRefError . P.tyRef .~ PC.toProto tr
               ]
    Just td -> return $ fromTyAbs (td ^. #tyAbs)

resolveTyVar :: MonadEval m => PC.TyVar -> m Ty
resolveTyVar tv = do
  args <- asks (view ctxTyArgs)
  case Map.lookup (PC.mkInfoLess $ tv ^. #varName) args of
    Nothing -> do
      mn <- asks (view ctxModuleName)
      throwError $
        defMessage
          & P.kindCheckErrors -- FIXME(bladyjoker): Parametrize error messages on 'stage' (KindChecking being one of them).
            .~ [ defMessage
                  & P.unboundTyVarError . P.moduleName .~ PC.toProto mn
                  & P.unboundTyVarError . P.tyVar .~ PC.toProto tv
               ]
    Just ty -> return ty

subst :: MonadEval m => Ty -> m Ty
subst (TyVar tv) = resolveTyVar tv
subst (TyAbs args' body _) = do
  args <- asks (view ctxTyArgs)
  let unbound = Map.difference args (OMap.toMap args')
  local (\ctx -> ctx & ctxTyArgs .~ unbound) (subst body)
subst (TyApp f args t) = TyApp <$> subst f <*> (subst `traverse` args) <*> pure t
subst (TySum ctors s) = TySum <$> (subst `traverse` ctors) <*> pure s
subst (TyTuple fields t) = TyTuple <$> (subst `traverse` fields) <*> pure t
subst (TyRecord fields r) = TyRecord <$> (subst `traverse` fields) <*> pure r
subst t = return t
