module LambdaBuffers.Codegen.Purescript.Print.LamVal (printValueE, printImplementation) where

import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (asks)
import Control.Monad.State.Class (gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.Purescript.Print.Names (printCtorName, printFieldName, printMkCtor, printPursQValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, enclose, encloseSep, equals, group, hsep, lbrace, lbracket, line, lparen, parens, rbrace, rbracket, rparen, space, vsep, (<+>))

newtype PrintRead = MkPrintRead
  { builtins :: Map LV.ValueName Purs.QValName
  }
  deriving stock (Show)

newtype PrintState = MkPrintState
  { currentVar :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype PrintCommand = ImportInstance Purs.ModuleName
  deriving stock (Eq, Ord)
  deriving stock (Show)

type PrintErr = String

type MonadPrint m = (MonadRWS PrintRead () PrintState m, MonadError PrintErr m)

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

printImplementation :: Map LV.ValueName Purs.QValName -> LV.ValueE -> Either PrintErr (Doc ann)
printImplementation lamValBuiltins valE =
  let p = runExcept $ runRWST (printValueE valE) (MkPrintRead lamValBuiltins) (MkPrintState 0)
   in case p of
        Left err -> Left err
        Right (doc, _, _) -> Right doc

printCtorCase :: MonadPrint m => PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const freshArg)
  argDocs <- for args printValueE
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE body
  let ctorNameDoc = printCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "->" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> hsep argDocs <+> "->" <+> group bodyDoc

printCaseE :: MonadPrint m => (PC.QTyName, LV.Sum) -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE (qtyN, sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE caseVal
  ctorCaseDocs <-
    vsep
      <$> for
        (OMap.assocs sumTy)
        ( \(cn, ty) -> case ty of -- TODO(bladyjoker): Cleanup by refactoring E.Ty.
            E.TyProduct fields _ -> printCtorCase qtyN ctorCont (cn, fields)
            _ -> throwError "TODO: Internal error, got a non-product in Sum."
        )
  return $ align $ "case" <+> caseValDoc <+> "of" <> line <> ctorCaseDocs

printLamE :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  arg <- freshArg
  bodyDoc <- printValueE (lamVal arg)
  argDoc <- printValueE arg
  return $ lparen <> "\\" <> argDoc <+> "->" <+> group bodyDoc <+> rparen

printAppE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  funDoc <- printValueE funVal
  argDoc <- printValueE argVal
  return $ funDoc <+> group (parens argDoc)

printFieldE :: MonadPrint m => LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE ((_, tyN), fieldN) recVal = do
  recDoc <- printValueE recVal
  case printFieldName (withInfo tyN) (withInfo fieldN) of
    Nothing -> throwError $ "TODO(bladyjoker): Internal error: Failed print a `FieldName` in Purescript implementation printer " <> show fieldN
    Just fnDoc -> return $ enclose lparen rparen ("Data.Newtype.unwrap" <+> recDoc) <> dot <> fnDoc -- TODO(bladyjoker): Import unwrap.

printLetE :: MonadPrint m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields) prodVal letCont = do
  letValDoc <- printValueE prodVal
  args <- for fields (const freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = printMkCtor (withInfo tyN)
  return $ "let" <+> prodCtorDoc <+> hsep argDocs <+> equals <+> letValDoc <+> "in" <+> bodyDoc

printOtherCase :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase otherCase = do
  arg <- freshArg
  argDoc <- printValueE arg
  bodyDoc <- printValueE $ otherCase arg
  return $ group $ argDoc <+> "->" <+> bodyDoc

-- | `caseInt [Tuple (fromInt 1) (\i -> i)] (\i -> i) (fromInt 1)`
printCaseIntE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  caseValDoc <- printValueE caseIntVal
  casesDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE conditionVal
          bodyDoc <- printValueE bodyVal
          return $ group $ "Data.Tuple.Tuple" <+> conditionDoc <+> parens bodyDoc
      )
  let casesDoc = lbracket <> align (encloseSep mempty mempty (comma <> space) casesDocs <> rbracket)
  otherDoc <- printValueE $ LV.LamE otherCase
  return $ "LambdaBuffers.Runtime.PlutusLedgerApi.caseInt" <+> casesDoc <+> otherDoc <+> caseValDoc

printListE :: MonadPrint m => [LV.ValueE] -> m (Doc ann)
printListE vals = do
  valDocs <- printValueE `traverse` vals
  return $ lbracket <> align (encloseSep mempty mempty (comma <> space) valDocs <> rbracket)

printCaseListE :: MonadPrint m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE caseListVal cases otherCase = do
  caseValDoc <- printValueE caseListVal
  caseDocs <-
    for
      cases
      ( \(listLength, bodyVal) -> do
          xs <- replicateM listLength freshArg
          conditionDoc <- printListE xs
          bodyDoc <- printValueE $ bodyVal xs
          return $ group $ conditionDoc <+> "->" <+> bodyDoc
      )
  otherDoc <- printOtherCase otherCase
  return $ "ca" <> align ("se" <+> caseValDoc <+> "of" <> line <> vsep (caseDocs <> [otherDoc]))

printCtorE :: MonadPrint m => LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE ((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals printValueE
  let ctorNDoc = printCtorName (withInfo tyN) (withInfo ctorN)
  if null prodDocs
    then return ctorNDoc
    else return $ ctorNDoc <+> align (hsep prodDocs)

printRecordE :: MonadPrint m => LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE ((_, tyN), _) vals = do
  fieldDocs <- for vals $
    \((fieldN, _), val) -> case printFieldName (withInfo tyN) (withInfo fieldN) of
      Nothing -> throwError "Failed printing field name"
      Just fieldNDoc -> do
        valDoc <- printValueE val
        return $ group $ fieldNDoc <+> colon <+> valDoc
  let ctorDoc = printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (lbrace <+> encloseSep mempty mempty (comma <> space) fieldDocs <+> rbrace)

printProductE :: MonadPrint m => LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE ((_, tyN), _) vals = do
  fieldDocs <- for vals printValueE
  let ctorDoc = printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (hsep fieldDocs)

printValueE :: MonadPrint m => LV.ValueE -> m (Doc ann)
printValueE (LV.VarE v) = return $ pretty v
printValueE (LV.RefE ref) = resolveRef ref
printValueE (LV.LamE lamVal) = printLamE lamVal
printValueE (LV.AppE funVal argVal) = printAppE funVal argVal
printValueE (LV.CaseE sumTy caseVal ctorCont) = printCaseE sumTy caseVal ctorCont
printValueE (LV.CtorE qctor prodVals) = printCtorE qctor prodVals
printValueE (LV.RecordE qrec vals) = printRecordE qrec vals
printValueE (LV.FieldE fieldName recVal) = printFieldE fieldName recVal
printValueE (LV.ProductE qprod vals) = printProductE qprod vals
printValueE (LV.LetE prodTy prodVal letCont) = printLetE prodTy prodVal letCont
printValueE (LV.IntE i) = return $ parens $ "Data.BigInt.fromInt" <+> pretty i -- TODO(bladyjoker): Import Data.BigInt.
printValueE (LV.CaseIntE intVal cases otherCase) = printCaseIntE intVal cases otherCase
printValueE (LV.ListE vals) = printListE vals
printValueE (LV.CaseListE listVal cases otherCase) = printCaseListE listVal cases otherCase
printValueE (LV.ErrorE err) = throwError $ "TODO(bladyjoker): LamVal error builtin was called " <> err

freshArg :: MonadPrint m => m LV.ValueE
freshArg = do
  i <- gets currentVar
  modify (\(MkPrintState s) -> MkPrintState $ s + 1)
  return $ LV.VarE $ "x" <> show i

{- | Resolves a `Ref` to a value reference in the target language.
 Resolves a `LV.Ref` which is a reference to a LamVal 'builtin', to the equivalent in the target language.
 If the `LV.Ref` is polymorphic like `eq @Int` or `eq @(Maybe a)` or `eq @(List Int)`,
 then lookup the implementation in the context and error if it's not there (TODO).

 NOTE(bladyjoker): Currently, this is assuming all the implementations are imported.
 TODO(bladyjoker): Output all necessary implementations from the Compiler and report on missing.
-}
resolveRef :: MonadPrint m => LV.Ref -> m (Doc ann)
resolveRef (_, refName) = do
  bs <- asks builtins
  case Map.lookup refName bs of
    Nothing -> throwError $ "TODO(bladyjoker): LamVal builtin mapping for " <> show refName <> " not configured."
    Just pqValName -> return $ printPursQValName pqValName -- TODO(bladyjoker): Add a TypeApplication notation?
