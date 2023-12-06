module LambdaBuffers.Codegen.Rust.Print.LamVal (printValueE, printInstance) where

import Control.Lens ((&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (replicateM)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.Codegen.Rust.Print.TyDef qualified as TD
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

phantomData :: R.QTyName
phantomData = R.qLibRef R.MkTyName "std" "marker" "PhantomData"

{- | Clone a value (converting a type to owned)
 As in codegen we cannot know whether a type is owned or borrowed, we make sure to have an owned type by making a clone.
 We must also borrow it first for the same reason (worst case it's a double && which Rust knows how to deal with)
-}
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

printCtorCase :: MonadPrint m => PC.TyDefs -> PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase iTyDefs (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
  argDocs <- for args (printValueE iTyDefs)
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE iTyDefs body
  let ctorNameDoc = R.printQualifiedCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "=>" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> encloseSep lparen rparen comma argDocs <+> "=>" <+> group bodyDoc

printCaseE :: MonadPrint m => PC.TyDefs -> LV.QSum -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE iTyDefs (qtyN@(_, tyN), sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE iTyDefs caseVal
  phantomCase <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs tyArgs (PC.SumI (PC.Sum ctor _)) _) _) -> do
        let phantomFields = TD.collectPhantomTyArgs (TD.sumCtorTys ctor) (toList tyArgs)
        if null phantomFields
          then return mempty
          else do
            let phantomCtor =
                  R.printTyName (withInfo tyN)
                    <> R.doubleColon
                    <> TD.phantomDataCtorIdent
                    <> encloseSep lparen rparen comma ("_" <$ phantomFields)
            return [phantomCtor <+> "=>" <+> "std::panic!(\"PhandomDataCtor should never be constructed.\")"]
      _ -> throwInternalError "Expected a SumE but got something else (TODO(szg251): Print got)"

  ctorCases <-
    for
      (OMap.assocs sumTy)
      ( \(cn, ty) -> case ty of
          LT.TyProduct fields _ -> printCtorCase iTyDefs qtyN ctorCont (cn, fields)
          _ -> throwInternalError "Got a non-product in Sum."
      )

  return $ "ma" <> align ("tch" <+> caseValDoc <+> braces (line <> vsep (punctuate comma (ctorCases <> phantomCase))))

{- | Prints a LamE lambda expression

 For greater compatibility, we also wrap the lambda in a `Box` and the arguments must always be
 borrowed references.

 ```rust
 Box::new(move |x0: &_| { <implBodyx> |)
 ```
-}
printLamE :: MonadPrint m => PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE iTyDefs lamVal = do
  arg <- LV.freshArg
  let body = lamVal arg
  bodyDoc <- printValueE iTyDefs body
  argDoc <- printValueE iTyDefs arg

  return $ "std::boxed::Box::new(move" <+> pipe <> argDoc <> colon <+> "&_" <> pipe <+> braces (space <> group bodyDoc) <> rparen

printAppE :: MonadPrint m => PC.TyDefs -> LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE iTyDefs funVal argVal = do
  funDoc <- printValueE iTyDefs funVal
  argDoc <- printValueE iTyDefs argVal
  return $ funDoc <> group (parens argDoc)

{- | Prints a record field accessor expression on a `ValueE` of type `Field`'.

 Given a LambdaBuffers module:
   module Foo

   record Foo a b = { foo : a, bar : b}

  `FieldE` on a field named `foo` of a record value `x` of type `Foo a b` translates into:

   &x.foo
-}
printFieldE :: MonadPrint m => PC.TyDefs -> LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE iTyDefs ((_, _), fieldN) recVal = do
  recDoc <- printValueE iTyDefs recVal
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
printLetE :: MonadPrint m => PC.TyDefs -> LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE iTyDefs (_, fields) prodVal letCont = do
  letValDoc <- printValueE iTyDefs prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args (printValueE iTyDefs)
  let bodyVal = letCont args
  bodyDoc <- printValueE iTyDefs bodyVal
  let letDocs = printLet letValDoc <$> zip argDocs [0 :: Int ..]
  return $ vsep letDocs <> line <> bodyDoc
  where
    printLet :: Doc ann -> (Doc ann, Int) -> Doc ann
    printLet letValDoc (arg, i) = "let" <+> arg <+> equals <+> borrow (letValDoc <> dot <> pretty i) <> semi

printOtherCase :: MonadPrint m => PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase iTyDefs otherCase = do
  arg <- LV.freshArg
  argDoc <- printValueE iTyDefs arg
  bodyDoc <- printValueE iTyDefs $ otherCase arg
  return $ group $ argDoc <+> "=>" <+> bodyDoc

--- | `printCaseIntE i [(1, x), (2,y)] (\other -> z)` translates into `LambdaBuffers.Runtime.Plutus.LamValcaseIntE i [(1,x), (2,y)] (\other -> z)`
printCaseIntE :: MonadPrint m => PC.TyDefs -> LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE iTyDefs caseIntVal cases otherCase = do
  caseIntERefDoc <- R.printRsQValName <$> LV.importValue caseIntERef
  _ <- LV.importValue bigInt
  caseValDoc <- printValueE iTyDefs caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE iTyDefs conditionVal
          bodyDoc <- printValueE iTyDefs bodyVal
          return $ group $ parens (fromU32 conditionDoc <> "," <+> bodyDoc)
      )
  otherDoc <- printLamE iTyDefs otherCase
  return $ group $ caseIntERefDoc <> encloseSep lparen rparen comma [caseValDoc, align (R.printRsQValName vecMacro <> encloseSep lbracket rbracket comma caseDocs), otherDoc]

printListE :: MonadPrint m => PC.TyDefs -> [LV.ValueE] -> m (Doc ann)
printListE iTyDefs vals = do
  valDocs <- printValueE iTyDefs `traverse` vals
  return $ brackets (align (encloseSep mempty mempty (comma <> space) valDocs))

printNewListE :: MonadPrint m => PC.TyDefs -> [LV.ValueE] -> m (Doc ann)
printNewListE iTyDefs vals = do
  lst <- printListE iTyDefs vals
  return $ R.printRsQValName vecMacro <> lst

printCaseListE :: MonadPrint m => PC.TyDefs -> LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE iTyDefs caseListVal cases otherCase = do
  caseValDoc <- printValueE iTyDefs caseListVal
  vecAsSliceDoc <- R.printRsQValName <$> LV.importValue vecAsSlice
  caseDocs <-
    for
      cases
      ( \(listLength, bodyVal) -> do
          xs <- replicateM listLength LV.freshArg
          conditionDoc <- printListE iTyDefs xs
          bodyDoc <- printValueE iTyDefs $ bodyVal xs
          return $ group $ conditionDoc <+> "=>" <+> bodyDoc
      )
  otherDoc <- printOtherCase iTyDefs otherCase
  return $ "ma" <> align ("tch" <+> vecAsSliceDoc <> parens caseValDoc <+> braces (line <> vsep (punctuate comma (caseDocs <> [otherDoc]))))

printCtorE :: MonadPrint m => PC.TyDefs -> LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE iTyDefs ((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals (printValueE iTyDefs)
  let ctorNDoc = R.printQualifiedCtorName (withInfo tyN) (withInfo ctorN)
  if null prodDocs
    then return ctorNDoc
    else return $ ctorNDoc <> align (encloseSep lparen rparen comma (clone <$> prodDocs))

printRecordE :: MonadPrint m => PC.TyDefs -> LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE iTyDefs ((_, tyN), _) vals = do
  fieldDocs <- for vals $
    \((fieldN, _), val) ->
      let fieldNDoc = R.printFieldName (withInfo fieldN)
       in do
            valDoc <- printValueE iTyDefs val
            return $ group $ fieldNDoc <> colon <+> clone valDoc
  let ctorDoc = R.printMkCtor (withInfo tyN)
  return $ ctorDoc <+> align (lbrace <+> encloseSep mempty mempty (comma <> space) fieldDocs <+> rbrace)

printProductE :: MonadPrint m => PC.TyDefs -> LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE iTyDefs (qtyN@(_, tyN), _) vals = do
  let ctorDoc = R.printMkCtor (withInfo tyN)
  fieldDocs <- for vals (printValueE iTyDefs)
  phantomFields <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs tyArgs (PC.ProductI (PC.Product fields _)) _) _) ->
        return $ R.printRsQTyName phantomData <$ TD.collectPhantomTyArgs fields (toList tyArgs)
      _ -> throwInternalError "Expected a ProductE but got something else (TODO(szg251): Print got)"
  return $ ctorDoc <> encloseSep lparen rparen comma ((clone <$> fieldDocs) <> phantomFields)

printTupleE :: MonadPrint m => PC.TyDefs -> LV.ValueE -> LV.ValueE -> m (Doc ann)
printTupleE iTyDefs l r = do
  lDoc <- printValueE iTyDefs l
  rDoc <- printValueE iTyDefs r
  return $ parens (lDoc <> comma <+> rDoc)

printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE = return . fromStr . dquotes . pretty

printCaseTextE :: (MonadPrint m) => PC.TyDefs -> LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE iTyDefs txtVal cases otherCase = do
  caseValDoc <- printValueE iTyDefs txtVal
  caseDocs <-
    for
      cases
      ( \case
          (LV.TextE caseTxt, bodyVal) -> do
            conditionDoc <- printTextE caseTxt
            bodyDoc <- printValueE iTyDefs bodyVal
            return $ group $ conditionDoc <+> "=>" <+> bodyDoc
          (_wrongCaseVal, _) -> throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )
  otherDoc <- printOtherCase iTyDefs otherCase
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

printValueE :: MonadPrint m => PC.TyDefs -> LV.ValueE -> m (Doc ann)
printValueE _ (LV.VarE v) = return $ pretty v
printValueE _ (LV.RefE ref) = printRefE ref
printValueE iTyDefs (LV.LamE lamVal) = printLamE iTyDefs lamVal
printValueE iTyDefs (LV.AppE funVal argVal) = printAppE iTyDefs funVal argVal
printValueE iTyDefs (LV.CaseE sumTy caseVal ctorCont) = printCaseE iTyDefs sumTy caseVal ctorCont
printValueE iTyDefs (LV.CtorE qctor prodVals) = printCtorE iTyDefs qctor prodVals
printValueE iTyDefs (LV.RecordE qrec vals) = printRecordE iTyDefs qrec vals
printValueE iTyDefs (LV.FieldE fieldName recVal) = printFieldE iTyDefs fieldName recVal
printValueE iTyDefs (LV.ProductE qprod vals) = printProductE iTyDefs qprod vals
printValueE iTyDefs (LV.LetE prodTy prodVal letCont) = printLetE iTyDefs prodTy prodVal letCont
printValueE _ (LV.IntE i) = return $ pretty i
printValueE iTyDefs (LV.CaseIntE intVal cases otherCase) = printCaseIntE iTyDefs intVal cases otherCase
printValueE iTyDefs (LV.ListE vals) = printNewListE iTyDefs vals
printValueE iTyDefs (LV.CaseListE listVal cases otherCase) = printCaseListE iTyDefs listVal cases otherCase
printValueE _ (LV.TextE txt) = printTextE txt
printValueE iTyDefs (LV.CaseTextE txtVal cases otherCase) = printCaseTextE iTyDefs txtVal cases otherCase
printValueE iTyDefs (LV.TupleE l r) = printTupleE iTyDefs l r
printValueE _ (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called " <> err

-- | This is a hack, to help Rust figure out types of closures (lambda expressions)
printInstance :: MonadPrint m => [R.QTyName] -> PC.TyDefs -> LV.ValueE -> m (Doc ann)
printInstance [] iTyDefs lamVal = printValueE iTyDefs lamVal
printInstance argTys iTyDefs (LV.LamE lamVal) = printInstanceLamE argTys iTyDefs lamVal
printInstance _ _ _ = throwInternalError "LamE expression expected with predefined argument types"

-- | This is a hack, to help Rust figure out types of closures (lambda expressions)
printInstanceLamE :: MonadPrint m => [R.QTyName] -> PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printInstanceLamE [] _ _ = throwInternalError "LamE expression expected with predefined argument types"
printInstanceLamE (argTy : argTys) iTyDefs lamVal = do
  arg <- LV.freshArg
  let body = lamVal arg
  bodyDoc <- printInstance argTys iTyDefs body
  argDoc <- printValueE iTyDefs arg
  let argTy' = "&'a" <+> R.printRsQTyName argTy

  return $ "std::boxed::Box::new(move" <+> pipe <> argDoc <> colon <+> argTy' <> pipe <+> braces (space <> group bodyDoc) <> rparen
