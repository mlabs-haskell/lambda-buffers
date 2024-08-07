module LambdaBuffers.Codegen.Haskell.Print.LamVal (printValueE, printLamE, printOtherCase, HaskellLamValContext (..), HaskellLamValMonad) where

import Control.Lens ((&), (.~))
import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS.Class (MonadReader (ask))
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, dquotes, encloseSep, equals, group, hsep, lbrace, lbracket, line, lparen, parens, rbrace, rbracket, rparen, space, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

newtype HaskellLamValContext = HaskellLamValContext
  { ctx'printCaseIntE :: forall m ann. HaskellLamValMonad m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
  }

type HaskellLamValMonad m = LV.MonadPrint m HsSyntax.QValName HaskellLamValContext

throwInternalError :: HaskellLamValMonad m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Haskell.Print.LamVal] " <> Text.pack msg

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

printCtorCase :: HaskellLamValMonad m => PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE body
  let ctorNameDoc = HsSyntax.printCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "->" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> hsep argDocs <+> "->" <+> group bodyDoc

printCaseE :: HaskellLamValMonad m => LV.QSum -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE (qtyN, sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE caseVal
  ctorCaseDocs <-
    vsep
      <$> for
        (OMap.assocs sumTy)
        ( \(cn, ty) -> case ty of -- TODO(bladyjoker): Cleanup by refactoring LT.Ty.
            LT.TyProduct fields _ -> printCtorCase qtyN ctorCont (cn, fields)
            _r -> throwInternalError "Got a non-product in Sum."
        )
  return $ "ca" <> align ("se" <+> caseValDoc <+> "of" <> line <> ctorCaseDocs)

printLamE :: HaskellLamValMonad m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  arg <- LV.freshArg
  bodyDoc <- printValueE (lamVal arg)
  argDoc <- printValueE arg
  return $ lparen <> "\\" <> argDoc <+> "->" <+> group bodyDoc <+> rparen

printAppE :: HaskellLamValMonad m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  funDoc <- printValueE funVal
  argDoc <- printValueE argVal
  return $ funDoc <+> group (parens argDoc)

{- | Prints a record field accessor expression on a `ValueE` of type `Field`'.

 Given a LambdaBuffers module:
   module Foo

   record Foo a b = { foo : a, bar : b}

  `FieldE` on a field named `foo` of a record value `x` of type `Foo a b` translates into:

   foo'foo x
-}
printFieldE :: HaskellLamValMonad m => LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE ((_, tyn), fieldN) recVal = do
  recDoc <- printValueE recVal
  let mayFnDoc = HsSyntax.printFieldName (withInfo tyn) (withInfo fieldN)
  case mayFnDoc of
    Nothing -> throwInternalError $ "Failed printing a `FieldName` " <> show fieldN
    Just fnDoc -> return $ fnDoc <+> recDoc

{- | Prints a `let` expression on a `ValueE` of type `Product`.

 Given a LambdaBuffers module:
   module Foo

   prod Foo a b = String a b

 `LetE` on `Foo a b` translates into:

   let MkFoo x1 x2 x3 = <letVal> in <letCont>
-}
printLetE :: HaskellLamValMonad m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields) prodVal letCont = do
  letValDoc <- printValueE prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = HsSyntax.printMkCtor (withInfo tyN)
  return $ "let" <+> prodCtorDoc <+> hsep argDocs <+> equals <+> letValDoc <+> "in" <+> bodyDoc

printOtherCase :: HaskellLamValMonad m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase otherCase = do
  arg <- LV.freshArg
  argDoc <- printValueE arg
  bodyDoc <- printValueE $ otherCase arg
  return $ group $ argDoc <+> "->" <+> bodyDoc

printListE :: HaskellLamValMonad m => [LV.ValueE] -> m (Doc ann)
printListE vals = do
  valDocs <- printValueE `traverse` vals
  return $ lbracket <> align (encloseSep mempty mempty (comma <> space) valDocs <> rbracket)

printCaseListE :: HaskellLamValMonad m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
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

printCtorE :: HaskellLamValMonad m => LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE ((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals printValueE
  let ctorNDoc = HsSyntax.printCtorName (withInfo tyN) (withInfo ctorN)
  if null prodDocs
    then return ctorNDoc
    else return $ ctorNDoc <+> align (hsep prodDocs)

printRecordE :: HaskellLamValMonad m => LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE ((_, tyN), _) vals = do
  fieldDocs <- for vals $
    \((fieldN, _), val) -> case HsSyntax.printFieldName (withInfo tyN) (withInfo fieldN) of
      Nothing -> throwInternalError $ "Failed printing field name " <> show fieldN
      Just fieldNDoc -> do
        valDoc <- printValueE val
        return $ group $ fieldNDoc <+> equals <+> valDoc
  let ctorDoc = HsSyntax.printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (lbrace <+> encloseSep mempty mempty (comma <> space) fieldDocs <+> rbrace)

printProductE :: HaskellLamValMonad m => LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE ((_, tyN), _) vals = do
  fieldDocs <- for vals printValueE
  let ctorDoc = HsSyntax.printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (hsep fieldDocs)

printTupleE :: HaskellLamValMonad m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printTupleE l r = do
  lDoc <- printValueE l
  rDoc <- printValueE r
  return $ parens (lDoc <> comma <> rDoc)

printTextE :: HaskellLamValMonad m => Text.Text -> m (Doc ann)
printTextE = return . dquotes . pretty

printCaseTextE :: HaskellLamValMonad m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE txtVal cases otherCase = do
  caseValDoc <- printValueE txtVal
  caseDocs <-
    for
      cases
      ( \case
          (LV.TextE caseTxt, bodyVal) -> do
            conditionDoc <- printTextE caseTxt
            bodyDoc <- printValueE bodyVal
            return $ group $ conditionDoc <+> "->" <+> bodyDoc
          (_wrongCaseVal, _) -> throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )
  otherDoc <- printOtherCase otherCase
  return $ "ca" <> align ("se" <+> caseValDoc <+> "of" <> line <> vsep (caseDocs <> [otherDoc]))

printRefE :: HaskellLamValMonad m => LV.Ref -> m (Doc ann)
printRefE ref = do
  qvn <- LV.resolveRef ref
  HsSyntax.printHsQValName <$> LV.importValue qvn

printVarE :: HaskellLamValMonad m => String -> m (Doc ann)
printVarE = return . pretty

printErrorE :: HaskellLamValMonad m => String -> m (Doc ann)
printErrorE err = throwInternalError $ "LamVal error builtin was called " <> err

printValueE :: HaskellLamValMonad m => LV.ValueE -> m (Doc ann)
printValueE (LV.VarE v) = printVarE v
printValueE (LV.RefE ref) = printRefE ref
printValueE (LV.LamE lamVal) = printLamE lamVal
printValueE (LV.AppE funVal argVal) = printAppE funVal argVal
printValueE (LV.CaseE sumTy caseVal ctorCont) = printCaseE sumTy caseVal ctorCont
printValueE (LV.CtorE qctor prodVals) = printCtorE qctor prodVals
printValueE (LV.RecordE qrec vals) = printRecordE qrec vals
printValueE (LV.FieldE fieldName recVal) = printFieldE fieldName recVal
printValueE (LV.ProductE qprod vals) = printProductE qprod vals
printValueE (LV.LetE prodTy prodVal letCont) = printLetE prodTy prodVal letCont
printValueE (LV.IntE i) = return $ pretty i
printValueE (LV.CaseIntE intVal cases otherCase) = do
  printCaseIntE <- ctx'printCaseIntE . LV.backendCtx <$> ask
  printCaseIntE intVal cases otherCase
printValueE (LV.ListE vals) = printListE vals
printValueE (LV.CaseListE listVal cases otherCase) = printCaseListE listVal cases otherCase
printValueE (LV.TextE txt) = printTextE txt
printValueE (LV.CaseTextE txtVal cases otherCase) = printCaseTextE txtVal cases otherCase
printValueE (LV.TupleE l r) = printTupleE l r
printValueE (LV.ErrorE err) = printErrorE err
