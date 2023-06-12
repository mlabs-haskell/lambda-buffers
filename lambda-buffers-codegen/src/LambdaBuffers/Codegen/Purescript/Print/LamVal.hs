module LambdaBuffers.Codegen.Purescript.Print.LamVal (printValueE) where

import Control.Lens ((&), (.~))
import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Purescript.Print.Names (printCtorName, printFieldName, printMkCtor, printPursQValName)
import LambdaBuffers.Codegen.Purescript.Syntax (normalValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, group, hsep, lbrace, lbracket, line, lparen, parens, rbrace, rbracket, rparen, space, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

type MonadPrint m = LV.MonadPrint m Purs.QValName

throwInternalError :: MonadPrint m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Purescript.Print.LamVal] " <> Text.pack msg

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

unwrap :: Purs.QValName
unwrap = normalValName "newtype" "Data.Newtype" "unwrap"

tuple :: Purs.QValName
tuple = normalValName "tuple" "Data.Tuple" "Tuple"

caseInt :: Purs.QValName
caseInt = normalValName "lb-purescript-runtime" "LambdaBuffers.Runtime.PlutusLedgerApi" "caseInt"

fromInt :: Purs.QValName
fromInt = normalValName "bigint" "Data.BigInt" "fromInt"

printCtorCase :: MonadPrint m => PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
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
        ( \(cn, ty) -> case ty of -- TODO(bladyjoker): Cleanup by refactoring LT.Ty.
            LT.TyProduct fields _ -> printCtorCase qtyN ctorCont (cn, fields)
            _ -> throwInternalError "Got a non-product in Sum."
        )
  return $ align $ "case" <+> caseValDoc <+> "of" <> line <> ctorCaseDocs

printLamE :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  arg <- LV.freshArg
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
    Nothing -> throwInternalError $ "Failed print a `FieldName` in Purescript implementation printer " <> show fieldN
    Just fnDoc -> do
      unwrapDoc <- printPursQValName <$> LV.importValue unwrap
      return $ parens (unwrapDoc <+> recDoc) <> dot <> fnDoc

printLetE :: MonadPrint m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields) prodVal letCont = do
  letValDoc <- printValueE prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = printMkCtor (withInfo tyN)
  return $ "let" <+> prodCtorDoc <+> hsep argDocs <+> equals <+> letValDoc <+> "in" <+> bodyDoc

printOtherCase :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase otherCase = do
  arg <- LV.freshArg
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
          tupleDoc <- printPursQValName <$> LV.importValue tuple
          return $ group $ tupleDoc <+> conditionDoc <+> parens bodyDoc
      )
  let casesDoc = lbracket <> align (encloseSep mempty mempty (comma <> space) casesDocs <> rbracket)
  otherDoc <- printValueE $ LV.LamE otherCase
  caseIntDoc <- printPursQValName <$> LV.importValue caseInt
  return $ caseIntDoc <+> casesDoc <+> otherDoc <+> caseValDoc

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
          xs <- replicateM listLength LV.freshArg
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
      Nothing -> throwInternalError "Failed printing field name"
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

printIntE :: MonadPrint m => Int -> m (Doc ann)
printIntE i = LV.importValue fromInt >>= (\fromIntDoc -> return $ parens $ fromIntDoc <+> pretty i) . printPursQValName

printRefE :: MonadPrint m => LV.Ref -> m (Doc ann)
printRefE ref = do
  qvn <- LV.resolveRef ref
  printPursQValName <$> LV.importValue qvn

printValueE :: MonadPrint m => LV.ValueE -> m (Doc ann)
printValueE (LV.VarE v) = return $ pretty v
printValueE (LV.RefE ref) = printRefE ref
printValueE (LV.LamE lamVal) = printLamE lamVal
printValueE (LV.AppE funVal argVal) = printAppE funVal argVal
printValueE (LV.CaseE sumTy caseVal ctorCont) = printCaseE sumTy caseVal ctorCont
printValueE (LV.CtorE qctor prodVals) = printCtorE qctor prodVals
printValueE (LV.RecordE qrec vals) = printRecordE qrec vals
printValueE (LV.FieldE fieldName recVal) = printFieldE fieldName recVal
printValueE (LV.ProductE qprod vals) = printProductE qprod vals
printValueE (LV.LetE prodTy prodVal letCont) = printLetE prodTy prodVal letCont
printValueE (LV.IntE i) = printIntE i
printValueE (LV.CaseIntE intVal cases otherCase) = printCaseIntE intVal cases otherCase
printValueE (LV.ListE vals) = printListE vals
printValueE (LV.CaseListE listVal cases otherCase) = printCaseListE listVal cases otherCase
printValueE (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called " <> err
printValueE _ = throwInternalError "TODO(bladyjoker) LamVal not implemented"
