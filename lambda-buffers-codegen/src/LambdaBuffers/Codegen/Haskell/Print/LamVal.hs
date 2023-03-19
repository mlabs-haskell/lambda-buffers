module LambdaBuffers.Codegen.Haskell.Print.LamVal (printValueE, printImplementation) where

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
import LambdaBuffers.Codegen.Haskell.Print.Names (printCtorName, printFieldName, printHsQValName, printMkCtor)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, enclose, equals, group, hsep, line, lparen, rparen, vsep, (<+>))

newtype PrintRead = MkPrintRead
  { builtins :: Map LV.ValueName H.QValName
  }
  deriving stock (Show)

newtype PrintState = MkPrintState
  { currentVar :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype PrintCommand = ImportInstance H.ModuleName
  deriving stock (Eq, Ord)
  deriving stock (Show)

type PrintErr = String

type MonadPrint m = (MonadRWS PrintRead () PrintState m, MonadError PrintErr m)

printImplementation :: Map LV.ValueName H.QValName -> LV.ValueE -> Either PrintErr (Doc ann)
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
  let ctorNameDoc = PC.withInfoLess tyn (PC.withInfoLess ctorN . printCtorName)
  return $ group $ ctorNameDoc <+> hsep argDocs <+> "->" <+> group bodyDoc

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
  return $ lparen <> "\\" <> argDoc <+> "->" <+> bodyDoc <+> rparen

printAppE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  funDoc <- printValueE funVal
  argDoc <- printValueE argVal
  return $ enclose lparen rparen $ funDoc <+> argDoc

{- | Prints a record field accessor expression on a `ValueE` of type `Field`'.

 Given a LambdaBuffers module:
   module Foo

   record Foo a b = { foo : a, bar : b}

  `FieldE` on a field named `foo` of a record value `x` of type `Foo a b` translates into:

   foo'foo x
-}
printFieldE :: MonadPrint m => (PC.QTyName, PC.InfoLess PC.FieldName) -> LV.ValueE -> m (Doc ann)
printFieldE ((_, tyn), fieldName) recVal = do
  recDoc <- printValueE recVal
  let mayFnDoc = PC.withInfoLess tyn (PC.withInfoLess fieldName . printFieldName)
  case mayFnDoc of
    Nothing -> throwError $ "TODO(bladyjoker): Internal error: Failed print a `FieldName` in Haskell implementation printer " <> show fieldName
    Just fnDoc -> return $ enclose lparen rparen (fnDoc <+> recDoc)

{- | Prints a `let` expression on a `ValueE` of type `Product`.

 Given a LambdaBuffers module:
   module Foo

   prod Foo a b = String a b

 `LetE` on `Foo a b` translates into:

   let MkFoo x1 x2 x3 = <letVal> in <letCont>
-}
printLetE :: MonadPrint m => (PC.QTyName, LV.Product, LV.ValueE) -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields, letVal) letCont = do
  letValDoc <- printValueE letVal
  args <- for fields (const freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = PC.withInfoLess tyN printMkCtor
  return $ "let" <+> prodCtorDoc <+> hsep argDocs <+> equals <+> letValDoc <+> "in" <+> bodyDoc

printValueE :: MonadPrint m => LV.ValueE -> m (Doc ann)
printValueE (LV.RefE ref) = resolveRef ref
printValueE (LV.CaseE sumTy caseVal ctorCont) = printCaseE sumTy caseVal ctorCont
printValueE (LV.LamE lamVal) = printLamE lamVal
printValueE (LV.AppE funVal argVal) = printAppE funVal argVal
printValueE (LV.VarE v) = return $ pretty v
printValueE (LV.FieldE fieldName recVal) = printFieldE fieldName recVal
printValueE (LV.LetE letVal letCont) = printLetE letVal letCont

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
resolveRef (mayTy, refName) = do
  bs <- asks builtins
  case Map.lookup refName bs of
    Nothing -> throwError $ "TODO(bladyjoker): LamVal builtin mapping for " <> show refName <> " not configured."
    Just hqValName -> case mayTy of
      Nothing -> return $ printHsQValName hqValName
      Just _ty -> return $ printHsQValName hqValName -- TODO(bladyjoker): Add a TypeApplication notation?
