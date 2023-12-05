module LambdaBuffers.Codegen.Rust.Print.LamVal (printValueE) where

import Control.Lens ((&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (replicateM)
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, angles, braces, brackets, colon, comma, dot, dquotes, encloseSep, equals, group, langle, lbrace, lbracket, line, lparen, parens, pipe, punctuate, rangle, rbrace, rbracket, rparen, semi, space, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

caseIntERef :: R.QValName
caseIntERef = R.Qualified'LibRef (R.MkCrateName "lbr-prelude") (R.MkModuleName "lamval") (R.MkValueName "case_int")

bigInt :: R.QValName
bigInt = R.Qualified'LibRef (R.MkCrateName "num-bigint") (R.MkModuleName "") (R.MkValueName "BigInt")

vecAsSlice :: R.QValName
vecAsSlice = R.Qualified'LibRef (R.MkCrateName "std") (R.MkModuleName "vec") (R.MkValueName "Vec::as_slice")

vecMacro :: R.QValName
vecMacro = R.Qualified'LibRef (R.MkCrateName "std") (R.MkModuleName "") (R.MkValueName "vec!")

fromU32Trait :: R.QTraitName
fromU32Trait = R.Qualified'LibRef (R.MkCrateName "std") (R.MkModuleName "convert") (R.MkTraitName "From<u32>")

fromStrTrait :: R.QTraitName
fromStrTrait = R.Qualified'LibRef (R.MkCrateName "std") (R.MkModuleName "convert") (R.MkTraitName "From<&str>")

cloneTrait :: R.QTraitName
cloneTrait = R.Qualified'LibRef (R.MkCrateName "std") (R.MkModuleName "clone") (R.MkTraitName "Clone")

clone :: Doc ann -> Doc ann
clone = useTraitMethod cloneTrait "clone" . borrow

borrow :: Doc ann -> Doc ann
borrow doc = "&" <> doc

fromU32 :: Doc ann -> Doc ann
fromU32 = useTraitMethod fromU32Trait "from"

fromStr :: Doc ann -> Doc ann
fromStr = useTraitMethod fromStrTrait "from"

useTraitMethod :: R.QTraitName -> Text -> Doc ann -> Doc ann
useTraitMethod trait method d = angles ("_" <+> "as" <+> R.printRsQTraitName trait) <> R.doubleColon <> pretty method <> parens d

throwInternalError :: MonadPrint m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Rust.Print.LamVal] " <> Text.pack msg

type MonadPrint m = LV.MonadPrint m R.QValName

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

printCtorCase :: MonadPrint m => PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE body
  let ctorNameDoc = R.printQualifiedCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "=>" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> encloseSep lparen rparen comma argDocs <+> "=>" <+> group bodyDoc

printCaseE :: MonadPrint m => LV.QSum -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE (qtyN, sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE caseVal
  ctorCaseDocs <-
    vsep . punctuate comma
      <$> for
        (OMap.assocs sumTy)
        ( \(cn, ty) -> case ty of
            LT.TyProduct fields _ -> printCtorCase qtyN ctorCont (cn, fields)
            _ -> throwInternalError "Got a non-product in Sum."
        )
  return $ "ma" <> align ("tch" <+> caseValDoc <+> braces (line <> ctorCaseDocs))

{- | Prints a LamE lambda expression

 For greater compatibility, we also wrap the lambda in a `Box` and the arguments must always be
 borrowed references. In case the closue returns another closure, the borrowed reference must have
 the lifetime of `'a`. This is not a bulletproof solution, but it works for our current
 LamVal implementations.

 ```rust
 Box::new(move |x0: &_| { <implBodyx> |)
 ```
-}
printLamE :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  arg <- LV.freshArg
  let body = lamVal arg
  bodyDoc <- printValueE body
  argDoc <- printValueE arg
  let argTy = case body of
        LV.LamE _ -> "&'a _"
        _ -> "&_"

  return $ "std::boxed::Box::new(move" <+> pipe <> argDoc <> colon <+> argTy <> pipe <+> braces (space <> group bodyDoc) <> rparen

printAppE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  funDoc <- printValueE funVal
  argDoc <- printValueE argVal
  return $ funDoc <> group (parens argDoc)

{- | Prints a record field accessor expression on a `ValueE` of type `Field`'.

 Given a LambdaBuffers module:
   module Foo

   record Foo a b = { foo : a, bar : b}

  `FieldE` on a field named `foo` of a record value `x` of type `Foo a b` translates into:

   &x.foo
-}
printFieldE :: MonadPrint m => LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE ((_, _), fieldN) recVal = do
  recDoc <- printValueE recVal
  let fnDoc = R.printFieldName (withInfo fieldN)
  return $ borrow $ recDoc <> dot <> fnDoc

{- | Prints a `let` expression on a `ValueE` of type `Product`.

 Given a LambdaBuffers module:
   module Foo

   prod Foo a b = String a b

 `LetE` on `Foo a b` translates into:

   let MkFoo(x1, x2, x3) = <letVal>;
   <letCont>
-}
printLetE :: MonadPrint m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields) prodVal letCont = do
  letValDoc <- printValueE prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = R.printMkCtor (withInfo tyN)
  return $ "let" <+> prodCtorDoc <> encloseSep lparen rparen comma argDocs <+> equals <+> letValDoc <> semi <> line <> bodyDoc

printOtherCase :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase otherCase = do
  arg <- LV.freshArg
  argDoc <- printValueE arg
  bodyDoc <- printValueE $ otherCase arg
  return $ group $ argDoc <+> "=>" <+> bodyDoc

--- | `printCaseIntE i [(1, x), (2,y)] (\other -> z)` translates into `LambdaBuffers.Runtime.Plutus.LamValcaseIntE i [(1,x), (2,y)] (\other -> z)`
printCaseIntE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  caseIntERefDoc <- R.printRsQValName <$> LV.importValue caseIntERef
  _ <- LV.importValue bigInt
  caseValDoc <- printValueE caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE conditionVal
          bodyDoc <- printValueE bodyVal
          return $ group $ parens (fromU32 conditionDoc <> "," <+> bodyDoc)
      )
  otherDoc <- printLamE otherCase
  return $ group $ caseIntERefDoc <> encloseSep lparen rparen comma [caseValDoc, align (R.printRsQValName vecMacro <> encloseSep lbracket rbracket comma caseDocs), otherDoc]

printListE :: MonadPrint m => [LV.ValueE] -> m (Doc ann)
printListE vals = do
  valDocs <- printValueE `traverse` vals
  return $ brackets (align (encloseSep mempty mempty (comma <> space) valDocs))

printNewListE :: MonadPrint m => [LV.ValueE] -> m (Doc ann)
printNewListE vals = do
  lst <- printListE vals
  return $ R.printRsQValName vecMacro <> lst

printCaseListE :: MonadPrint m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE caseListVal cases otherCase = do
  caseValDoc <- printValueE caseListVal
  vecAsSliceDoc <- R.printRsQValName <$> LV.importValue vecAsSlice
  caseDocs <-
    for
      cases
      ( \(listLength, bodyVal) -> do
          xs <- replicateM listLength LV.freshArg
          conditionDoc <- printListE xs
          bodyDoc <- printValueE $ bodyVal xs
          return $ group $ conditionDoc <+> "=>" <+> bodyDoc
      )
  otherDoc <- printOtherCase otherCase
  return $ "ma" <> align ("tch" <+> vecAsSliceDoc <> parens caseValDoc <+> braces (line <> vsep (punctuate comma (caseDocs <> [otherDoc]))))

printCtorE :: MonadPrint m => LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE ((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals printValueE
  let ctorNDoc = R.printQualifiedCtorName (withInfo tyN) (withInfo ctorN)
  if null prodDocs
    then return ctorNDoc
    else return $ ctorNDoc <> align (encloseSep lparen rparen comma (clone <$> prodDocs))

printRecordE :: MonadPrint m => LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE ((_, tyN), _) vals = do
  fieldDocs <- for vals $
    \((fieldN, _), val) ->
      let fieldNDoc = R.printFieldName (withInfo fieldN)
       in do
            valDoc <- printValueE val
            return $ group $ fieldNDoc <> colon <+> clone valDoc
  let ctorDoc = R.printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (lbrace <+> encloseSep mempty mempty (comma <> space) fieldDocs <+> rbrace)

printProductE :: MonadPrint m => LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE ((_, tyN), _) vals = do
  fieldDocs <- for vals printValueE
  let ctorDoc = R.printMkCtor (withInfo tyN)
  return $ ctorDoc <> encloseSep lparen rparen comma (clone <$> fieldDocs)

printTupleE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printTupleE l r = do
  lDoc <- clone <$> printValueE l
  rDoc <- clone <$> printValueE r
  return $ parens (lDoc <> comma <+> rDoc)

printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE = return . fromStr . dquotes . pretty

printCaseTextE :: (MonadPrint m) => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE txtVal cases otherCase = do
  caseValDoc <- printValueE txtVal
  caseDocs <-
    for
      cases
      ( \case
          (LV.TextE caseTxt, bodyVal) -> do
            conditionDoc <- printTextE caseTxt
            bodyDoc <- printValueE bodyVal
            return $ group $ conditionDoc <+> "=>" <+> bodyDoc
          (_wrongCaseVal, _) -> throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )
  otherDoc <- printOtherCase otherCase
  return $ "ma" <> align ("tch" <+> caseValDoc <+> braces (line <> vsep (punctuate comma (caseDocs <> [otherDoc]))))

printRefE :: MonadPrint m => LV.Ref -> m (Doc ann)
printRefE ref = do
  qvn <- LV.resolveRef ref
  case ref of
    (argTy : _, builtin)
      | builtin == "toPlutusData"
          || builtin == "fromPlutusData"
          || builtin == "toJson"
          || builtin == "fromJson" -> do
          lamTyDoc <- printLamTy argTy
          methodDoc <- R.printRsValName . R.qualifiedEntity <$> LV.importValue qvn
          return $ angles lamTyDoc <> R.doubleColon <> methodDoc
      | builtin == "jsonConstructor" -> do
          lamTyDoc <- printLamTy argTy
          methodDoc <- R.printRsQValName <$> LV.importValue qvn
          return $ methodDoc <> R.doubleColon <> langle <> lamTyDoc <> rangle
    _ -> R.printRsQValName <$> LV.importValue qvn

printLamTy :: MonadPrint m => LT.Ty -> m (Doc ann)
printLamTy (LT.TyRef tyRef) = return $ R.printTyRef tyRef
printLamTy (LT.TyVar tyVar) = return $ R.printTyVar tyVar
printLamTy (LT.TyApp ty gens _) = do
  tyDoc <- printLamTy ty
  gensDoc <- for gens printLamTy
  return $ tyDoc <> R.encloseGenerics gensDoc
printLamTy _ = throwInternalError "Unexpected LamTy expression."

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
printValueE (LV.IntE i) = return $ pretty i
printValueE (LV.CaseIntE intVal cases otherCase) = printCaseIntE intVal cases otherCase
printValueE (LV.ListE vals) = printNewListE vals
printValueE (LV.CaseListE listVal cases otherCase) = printCaseListE listVal cases otherCase
printValueE (LV.TextE txt) = printTextE txt
printValueE (LV.CaseTextE txtVal cases otherCase) = printCaseTextE txtVal cases otherCase
printValueE (LV.TupleE l r) = printTupleE l r
printValueE (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called " <> err
