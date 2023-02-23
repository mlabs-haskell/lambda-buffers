module LambdaBuffers.Codegen.Haskell (printTyDef, printSum, CabalPackageName (..), HaskModuleName (..), HaskTyName (..)) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (MonadReader (local), MonadWriter (tell))
import Control.Monad.RWS.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Generics.Labels ()

-- TODO(bladyjoker): Remove this when possible
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types (ConstrName (ConstrName), Constructor (Constructor), Field (Field), FieldName (FieldName), ForeignRef (ForeignRef), LocalRef (LocalRef), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (RecordI, TupleI), Record (Record), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyAbs (TyAbs), TyApp (TyApp), TyArg (TyArg), TyBody (OpaqueI, SumI), TyDef, TyName (TyName), TyRef (ForeignI, LocalI), TyVar (TyVar), VarName (VarName))
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, equals, group, lbrace, pipe, rbrace, sep, space, surround, (<+>))

newtype CabalPackageName = MkCabalPackageName Text
newtype HaskModuleName = MkHaskModuleName Text
newtype HaskTyName = MkHaskTyName Text
type QualifiedHaskTyRef = (CabalPackageName, HaskModuleName, HaskTyName)

newtype PrintConfig = PrintConfig
  { _opaques :: Map TyName QualifiedHaskTyRef
  }

newtype PrintCtx = TyDefCtx TyDef
type PrintRead = (PrintConfig, PrintCtx)

type PrintWrite = [PrintCommand]
data PrintCommand = PrintTyDef (Doc ()) | AddImport QualifiedHaskTyRef | AddTyExport HaskTyName
type PrintErr = String

-- FIXME(bladyjoker): I need to keep the ordering on Constructors, Fields and TyArgs.

askConfig :: MonadReader PrintRead m => m PrintConfig
askConfig = asks fst

askCtx :: MonadReader PrintRead m => m PrintCtx
askCtx = asks snd

askTyDef :: MonadReader PrintRead m => m TyDef
askTyDef = do
  ctx <- askCtx
  case ctx of
    TyDefCtx td -> return td

printTyDef :: (MonadWriter PrintWrite m, MonadReader PrintRead m, MonadError PrintErr m) => TyDef -> m ()
printTyDef td = do
  local (\(cfg, _) -> (cfg, TyDefCtx td)) (printTyAbs $ td ^. #tyAbs)

printTyAbs :: (MonadWriter PrintWrite m, MonadReader PrintRead m, MonadError PrintErr m) => TyAbs -> m ()
printTyAbs (TyAbs _ (OpaqueI _) _) = do
  PrintConfig cfg <- askConfig
  tn <- view #tyName <$> askTyDef
  qhsRef@(_, _, hsTyName) <- case Map.lookup tn cfg of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show tn
    Just qhsRef -> return qhsRef
  tell
    [ AddTyExport hsTyName
    , AddImport qhsRef
    ]
printTyAbs (TyAbs args (SumI s) _) = do
  sumDoc <- printSum s
  td <- askTyDef
  let tdDoc = group $ pretty (td ^. #tyName . #name) <+> sep (printTyArg <$> toList args) <+> equals <+> sumDoc
  tell
    [ AddTyExport (MkHaskTyName $ td ^. #tyName . #name)
    , PrintTyDef tdDoc
    ]

printTyArg :: forall {a}. TyArg -> Doc a
printTyArg (TyArg vn _ _) = printVarName vn

printSum :: Monad m => Sum -> m (Doc ())
printSum (Sum ctors _) =
  return $
    group $
      if null ctors
        then mempty
        else align $ encloseSep mempty mempty (space <> pipe <> space) (printCtor <$> toList ctors)

printCtor :: Constructor -> Doc a
printCtor (Constructor ctorName prod) = align $ group (printCtorName ctorName <> printProd prod)

printCtorName :: ConstrName -> Doc a
printCtorName (ConstrName n _) = pretty n -- TODO(bladyjoker): Constructor name is formed as SumTyName'ConstrName

printProd :: Product -> Doc a
printProd (RecordI rc) = printRec rc
printProd (TupleI tup) = printTup tup

printRec :: Record -> Doc a
printRec (Record fields _) = group $ encloseSep lbrace rbrace (space <> comma <> space) (printField <$> toList fields)

printTup :: Tuple -> Doc a
printTup (Tuple fields _si) = group $ sep (printTy <$> fields)

printField :: Field -> Doc a
printField (Field fn ty) = printFieldName fn <+> colon <> colon <+> printTy ty -- -- TODO(bladyjoker): Field name is formed as recName'fieldName

printTy :: Ty -> Doc a
printTy (TyVarI v) = printTyVar v
printTy (TyRefI r) = printTyRef r
printTy (TyAppI a) = printTyApp a

printTyApp :: TyApp -> Doc a
printTyApp (TyApp f args _) = group $ printTy f <+> align (sep (printTy <$> args))

printTyRef :: TyRef -> Doc a
printTyRef (LocalI (LocalRef tn _)) = group $ printTyName tn
printTyRef (ForeignI (ForeignRef tn mn _)) = group $ printModName mn <> dot <> printTyName tn -- TODO(bladyjoker): Emit Import

printTyVar :: TyVar -> Doc a
printTyVar (TyVar vn _) = printVarName vn

printVarName :: VarName -> Doc a
printVarName (VarName n _) = pretty n

printFieldName :: FieldName -> Doc a
printFieldName (FieldName n _) = pretty n

printTyName :: TyName -> Doc a
printTyName (TyName n _) = pretty n

printModName :: ModuleName -> Doc a
printModName (ModuleName parts _) = group $ concatWith (surround dot) [pretty p | ModuleNamePart p _ <- parts]
