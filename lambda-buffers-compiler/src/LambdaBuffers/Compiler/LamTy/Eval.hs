module LambdaBuffers.Compiler.LamTy.Eval (runEval, runEval', eval) where

import Control.Lens (makeLenses, view, (%~), (&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.LamTy.Pretty ()
import LambdaBuffers.Compiler.LamTy.Types (Ty (TyAbs, TyApp, TyProduct, TyRecord, TyRef, TySum, TyVar), fromTy, fromTyAbs)
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data Context = MkContext
  { _ctxModuleName :: PC.ModuleName
  , _ctxTyDefs :: Map PC.QTyName PC.TyDef
  , _ctxTyArgs :: Map (PC.InfoLess PC.VarName) Ty
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkContext

type MonadEval m = (MonadError P.Error m, MonadReader Context m)

runEval :: PC.ModuleName -> Map PC.QTyName PC.TyDef -> PC.Ty -> Either P.Error Ty
runEval mn tds ty = runEval' mn tds (fromTy ty)

runEval' :: PC.ModuleName -> Map PC.QTyName PC.TyDef -> Ty -> Either P.Error Ty
runEval' mn tds ty =
  let p = runReaderT (eval ty) (MkContext mn tds mempty)
   in runExcept p

eval :: MonadEval m => Ty -> m Ty
eval (TyRef tr) = eval (TyApp (TyRef tr) [] Nothing)
eval (TyApp (TyRef tr) args t) = resolveTyRef tr >>= (\f -> eval $ TyApp f args t)
eval (TyApp (TyAbs args body _) [] _) | args == OMap.empty = return body
eval (TyApp (TyAbs args' body _) tys _) =
  let actx' = Map.fromList $ zip (PC.mkInfoLess . view #argName <$> toList args') tys
   in local (\ctx -> ctx & ctxTyArgs %~ Map.union actx') (subst body)
eval (TyApp f args t) = TyApp f <$> eval `traverse` args <*> pure t
eval (TySum ctors s) = TySum <$> (eval `traverse` ctors) <*> pure s
eval (TyProduct fields t) = TyProduct <$> (eval `traverse` fields) <*> pure t
eval (TyRecord fields r) = TyRecord <$> (eval `traverse` fields) <*> pure r
eval t = pure t

subst :: MonadEval m => Ty -> m Ty
subst (TyVar tv) = resolveTyVar tv
subst (TyAbs args' body _) = do
  args <- asks (view ctxTyArgs)
  let unbound = Map.difference args (OMap.toMap args')
  local (\ctx -> ctx & ctxTyArgs .~ unbound) (subst body)
subst (TyApp f args t) = TyApp <$> subst f <*> (subst `traverse` args) <*> pure t
subst (TySum ctors s) = TySum <$> (subst `traverse` ctors) <*> pure s
subst (TyProduct fields t) = TyProduct <$> (subst `traverse` fields) <*> pure t
subst (TyRecord fields r) = TyRecord <$> (subst `traverse` fields) <*> pure r
subst t = return t

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
