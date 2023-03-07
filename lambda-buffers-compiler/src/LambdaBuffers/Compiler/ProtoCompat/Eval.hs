module LambdaBuffers.Compiler.ProtoCompat.Eval (Ty (..), fromTy, eval, runEval, runEval', prettyTy) where

import Control.Lens (makeLenses, view, (%~), (&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), Pretty (pretty), concatWith, dot, enclose, encloseSep, hsep, layoutPretty, lparen, pipe, rparen, space, (<+>))
import Prettyprinter.Render.String (renderShowS)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data Ty
  = TyApp Ty [Ty] (Maybe PC.TyApp)
  | TyAbs (OMap (PC.InfoLess PC.VarName) PC.TyArg) Ty PC.TyAbs
  | TyRef PC.TyRef
  | TyVar PC.TyVar
  | TyOpaque PC.SourceInfo
  | TySum (OMap (PC.InfoLess PC.ConstrName) Ty) PC.Sum
  | TyTuple [Ty] PC.Product
  | TyRecord (OMap (PC.InfoLess PC.FieldName) Ty) PC.Product
  deriving stock (Eq, Ord)

prettyTy :: Ty -> Doc a
prettyTy (TyVar tv) = pretty $ tv ^. #varName . #name
prettyTy (TyAbs args body _) =
  if null args
    then prettyTy body
    else enclose lparen rparen $ printArgs args <+> "->" <+> prettyTy body
  where
    printArgs :: OMap (PC.InfoLess PC.VarName) PC.TyArg -> Doc a
    printArgs args' = hsep (pretty . view (#argName . #name) <$> toList args')
prettyTy (TyApp f args _t) = if null args then prettyTy f else encloseSep lparen rparen space (prettyTy <$> f : args)
prettyTy (TySum ctors _s) = enclose lparen rparen $ sepWith (space <> pipe <> space) (fmap prettyCtor . OMap.assocs $ ctors)
  where
    prettyCtor :: (PC.InfoLess PC.ConstrName, Ty) -> Doc a
    prettyCtor (cn, p) = pretty (PC.withInfoLess cn (view #name)) <+> prettyTy p
prettyTy (TyTuple fields _t) = hsep $ prettyTy <$> fields
prettyTy (TyRecord fields _r) = hsep $ prettyTy <$> toList fields
prettyTy (TyOpaque _) = "opq"
prettyTy (TyRef (PC.LocalI lr)) = pretty $ lr ^. #tyName . #name
prettyTy (TyRef (PC.ForeignI fr)) = prettyMn (fr ^. #moduleName) <> dot <> pretty (fr ^. #tyName . #name)
  where
    prettyMn :: PC.ModuleName -> Doc a
    prettyMn mn = encloseSep mempty mempty dot $ pretty . view #name <$> mn ^. #parts

sepWith :: Foldable t => Doc ann -> t (Doc ann) -> Doc ann
sepWith d = concatWith (\l r -> l <> d <> r)

instance Pretty Ty where
  pretty = prettyTy

instance Show Ty where
  showsPrec _ = renderShowS . layoutPretty (LayoutOptions Unbounded) . pretty

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

runEval :: PC.ModuleName -> Map PC.QTyName PC.TyDef -> PC.Ty -> Either P.CompilerError Ty
runEval mn tds ty = runEval' mn tds (fromTy ty)

runEval' :: PC.ModuleName -> Map PC.QTyName PC.TyDef -> Ty -> Either P.CompilerError Ty
runEval' mn tds ty =
  let p = runReaderT (eval ty) (MkContext mn tds mempty)
   in runExcept p

eval :: MonadEval m => Ty -> m Ty
eval (TyRef tr) = resolveTyRef tr >>= eval
eval (TyApp (TyRef tr) args t) = resolveTyRef tr >>= (\f -> eval $ TyApp f args t)
eval (TyApp (TyAbs args' body _) tys _) =
  let actx' = Map.fromList $ zip (PC.mkInfoLess . view #argName <$> toList args') tys
   in local (\ctx -> ctx & ctxTyArgs %~ Map.union actx') (subst body)
eval (TyApp f args t) = TyApp f <$> eval `traverse` args <*> pure t
eval (TySum ctors s) = TySum <$> (eval `traverse` ctors) <*> pure s
eval (TyTuple fields t) = TyTuple <$> (eval `traverse` fields) <*> pure t
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
subst (TyTuple fields t) = TyTuple <$> (subst `traverse` fields) <*> pure t
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
