module LambdaBuffers.Codegen.Rust.Print.LamVal (printValueE, printInstance) where

import Control.Lens ((&), (.~))
import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Rust.Print.Refs qualified as RR
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.Codegen.Rust.Print.TyDef qualified as TD
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, angles, braces, brackets, colon, comma, dot, dquotes, encloseSep, equals, group, langle, lbracket, line, lparen, parens, pipe, punctuate, rangle, rbracket, rparen, semi, space, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

type MonadPrint m = LV.MonadPrint m R.QValName ()

toOwned :: Doc ann -> Doc ann
toOwned = useTraitMethod RR.toOwnedTrait "to_owned"

borrow :: Doc ann -> Doc ann
borrow doc = "&" <> doc

fromU32 :: Doc ann -> Doc ann
fromU32 = useTraitMethod RR.fromU32Trait "from"

fromStr :: Doc ann -> Doc ann
fromStr = useTraitMethod RR.fromStrTrait "from"

useTraitMethod :: R.QTraitName -> Text -> Doc ann -> Doc ann
useTraitMethod trait method d = angles ("_" <+> "as" <+> R.printRsQTraitName trait) <> R.doubleColon <> pretty method <> parens d

throwInternalError :: MonadPrint m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Rust.Print.LamVal] " <> Text.pack msg

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

printCtorCase :: MonadPrint m => R.PkgMap -> PC.TyDefs -> PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase pkgs iTyDefs (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
  argDocs <- for args (printValueE pkgs iTyDefs)
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE pkgs iTyDefs body
  let ctorNameDoc = R.printQualifiedCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "=>" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> encloseSep lparen rparen comma argDocs <+> "=>" <+> group bodyDoc

printCaseE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QSum -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE pkgs iTyDefs (qtyN@(mn', tyN'), sumTy) caseVal ctorCont = do
  let mn = withInfo mn'
      tyN = withInfo tyN'
  caseValDoc <- printValueE pkgs iTyDefs caseVal
  (tyArgs, fieldTys) <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs tyArgs (PC.SumI (PC.Sum ctors _)) _) _) -> do
        return (toList tyArgs, TD.sumCtorTys ctors)
      _other -> throwInternalError "Expected a SumE but got something else (TODO(szg251): Print got)"

  let phantomFields = TD.collectPhantomTyArgs iTyDefs mn tyN fieldTys tyArgs
      phantomCaseDoc =
        if null phantomFields
          then mempty
          else
            let phantomCtor =
                  R.printTyName tyN
                    <> R.doubleColon
                    <> TD.phantomDataCtorIdent
                    <> encloseSep lparen rparen comma ("_" <$ phantomFields)
             in [phantomCtor <+> "=>" <+> "std::panic!(\"PhandomDataCtor should never be constructed.\")"]

  ctorCases <-
    for
      (OMap.assocs sumTy)
      ( \(cn, ty) -> case ty of
          LT.TyProduct fields _other -> printCtorCase pkgs iTyDefs qtyN ctorCont (cn, fields)
          _other -> throwInternalError "Got a non-product in Sum."
      )

  return $ "ma" <> align ("tch" <+> caseValDoc <+> braces (line <> vsep (punctuate comma (ctorCases <> phantomCaseDoc))))

{- | Prints a LamE lambda expression

 For greater compatibility, we also wrap the lambda in a `Box` and the arguments must always be
 borrowed references.

 ```rust
 std::boxed::Box::new(move |x0: &_| { <implBodyx> |)
 ```
-}
printLamE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE pkgs iTyDefs lamVal = do
  arg <- LV.freshArg
  let body = lamVal arg
  bodyDoc <- printValueE pkgs iTyDefs body
  argDoc <- printValueE pkgs iTyDefs arg

  return $ R.printRsQValName RR.boxNew <> parens ("move" <+> pipe <> argDoc <> colon <+> "&_" <> pipe <+> braces (space <> group bodyDoc))

printAppE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE pkgs iTyDefs funVal argVal = do
  funDoc <- printValueE pkgs iTyDefs funVal
  argDoc <- printValueE pkgs iTyDefs argVal
  return $ funDoc <> group (parens argDoc)

{- | Prints a record field accessor expression on a `ValueE` of type `Field`'.

 Given a LambdaBuffers module:
   module Foo

   record Foo a b = { foo : a, bar : b}

  `FieldE` on a field named `foo` of a record value `x` of type `Foo a b` translates into:

   &x.foo
-}
printFieldE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE pkgs iTyDefs ((_, _), fieldN) recVal = do
  recDoc <- printValueE pkgs iTyDefs recVal
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
printLetE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE pkgs iTyDefs (_, fields) prodVal letCont = do
  letValDoc <- printValueE pkgs iTyDefs prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args (printValueE pkgs iTyDefs)
  let bodyVal = letCont args
  bodyDoc <- printValueE pkgs iTyDefs bodyVal
  let letDocs = printLet letValDoc <$> zip argDocs [0 :: Int ..]
  return $ vsep letDocs <> line <> bodyDoc
  where
    printLet :: Doc ann -> (Doc ann, Int) -> Doc ann
    printLet letValDoc (arg, i) = "let" <+> arg <+> equals <+> borrow (letValDoc <> dot <> pretty i) <> semi

printOtherCase :: MonadPrint m => R.PkgMap -> PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase pkgs iTyDefs otherCase = do
  arg <- LV.freshArg
  argDoc <- printValueE pkgs iTyDefs arg
  bodyDoc <- printValueE pkgs iTyDefs $ otherCase arg
  return $ group $ argDoc <+> "=>" <+> bodyDoc

--- | Call `case_int` function `lbr_prelude`
--
-- `printCaseIntE i [(1, x), (2,y)] (\other -> z)` translates into `lbr_prelude::prelude::case_int(i, vec![(1, x), (2,y)]), Box::new(move |other| z)`
printCaseIntE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE pkgs iTyDefs caseIntVal cases otherCase = do
  caseIntERefDoc <- R.printRsQValName <$> LV.importValue RR.caseIntERef
  _ <- LV.importValue RR.bigInt
  caseValDoc <- printValueE pkgs iTyDefs caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE pkgs iTyDefs conditionVal
          bodyDoc <- printValueE pkgs iTyDefs bodyVal
          return $ group $ parens (fromU32 conditionDoc <> "," <+> bodyDoc)
      )
  otherDoc <- printLamE pkgs iTyDefs otherCase
  return $ group $ caseIntERefDoc <> encloseSep lparen rparen comma [caseValDoc, align (R.printRsQValName RR.vecMacro <> encloseSep lbracket rbracket comma caseDocs), otherDoc]

{- | Print a list of values
 ```
 [<A>, <B>]
 ```
-}
printListE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> [LV.ValueE] -> m (Doc ann)
printListE pkgs iTyDefs vals = do
  valDocs <- printValueE pkgs iTyDefs `traverse` vals
  return $ brackets (align (encloseSep mempty mempty (comma <> space) valDocs))

{- | Print a list (Vector in Rust) construction using vec! macro
 ```
 std::vec![<A>, <B>]
 ```
-}
printNewListE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> [LV.ValueE] -> m (Doc ann)
printNewListE pkgs iTyDefs vals = do
  lst <- printListE pkgs iTyDefs vals
  return $ R.printRsQValName RR.vecMacro <> lst

printCaseListE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE pkgs iTyDefs caseListVal cases otherCase = do
  caseValDoc <- printValueE pkgs iTyDefs caseListVal
  vecAsSliceDoc <- R.printRsQValName <$> LV.importValue RR.vecAsSlice
  caseDocs <-
    for
      cases
      ( \(listLength, bodyVal) -> do
          xs <- replicateM listLength LV.freshArg
          conditionDoc <- printListE pkgs iTyDefs xs
          bodyDoc <- printValueE pkgs iTyDefs $ bodyVal xs
          return $ group $ conditionDoc <+> "=>" <+> bodyDoc
      )
  otherDoc <- printOtherCase pkgs iTyDefs otherCase
  return $ "ma" <> align ("tch" <+> vecAsSliceDoc <> parens caseValDoc <+> braces (line <> vsep (punctuate comma (caseDocs <> [otherDoc]))))

printCtorE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE pkgs iTyDefs (qtyN@(mn', tyN'), (ctorN, _)) prodVals = do
  let mn = withInfo mn'
      tyN = withInfo tyN'

  fieldTys <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs _ (PC.SumI (PC.Sum ctors _)) _) _) -> do
        case OMap.lookup ctorN ctors of
          Just (PC.Constructor _ (PC.Product tys _)) -> return tys
          Nothing -> throwInternalError "Couldn't find ConstrName in the TyDefs"
      _other -> throwInternalError "Expected a SumE but got something else (TODO(szg251): Print got)"

  let ctorNDoc = R.printQualifiedCtorName tyN (withInfo ctorN)
      mayBoxedFields = zip prodVals $ TD.isRecursive iTyDefs mn tyN <$> fieldTys

  prodDocs <- for mayBoxedFields (printMaybeBoxed pkgs iTyDefs)
  if null prodDocs
    then return ctorNDoc
    else return $ ctorNDoc <> align (encloseSep lparen rparen comma prodDocs)

printRecordE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE pkgs iTyDefs (qtyN@(mn', tyN'), _) vals = do
  let mn = withInfo mn'
      tyN = withInfo tyN'

  let ctorDoc = R.printMkCtor tyN
  (tyArgs, fieldTys) <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs tyArgs (PC.RecordI (PC.Record fields _)) _) _) -> return (toList tyArgs, TD.recFieldTys fields)
      _other -> throwInternalError "Expected a RecordE but got something else (TODO(szg251): Print got)"

  let phantomFields = TD.collectPhantomTyArgs iTyDefs mn tyN fieldTys tyArgs
      phantomFieldDocs =
        if null phantomFields
          then mempty
          else printPhantomDataField <$> phantomFields
      mayBoxedFields = zip (sortOn fst vals) $ TD.isRecursive iTyDefs mn tyN <$> fieldTys

  fieldDocs <- for mayBoxedFields $
    \(((fieldN, _), val), isBoxed) ->
      let fieldNDoc = R.printFieldName (withInfo fieldN)
       in do
            valDoc <- printMaybeBoxed pkgs iTyDefs (val, isBoxed)
            return $ group $ fieldNDoc <> colon <+> valDoc

  return $ ctorDoc <+> align (braces (line <> vsep (punctuate comma (fieldDocs <> phantomFieldDocs)) <> line))

printPhantomDataField :: PC.TyArg -> Doc ann
printPhantomDataField tyArg =
  TD.phantomFieldIdent tyArg <> colon <+> R.printRsQTyName RR.phantomData

printProductE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE pkgs iTyDefs (qtyN@(mn', tyN'), _) vals = do
  let mn = withInfo mn'
      tyN = withInfo tyN'

  let ctorDoc = R.printMkCtor tyN
  (tyArgs, fieldTys) <-
    case Map.lookup qtyN iTyDefs of
      Just (PC.TyDef _ (PC.TyAbs tyArgs (PC.ProductI (PC.Product fields _)) _) _) -> return (toList tyArgs, fields)
      _other -> throwInternalError "Expected a ProductE but got something else (TODO(szg251): Print got)"
  let phantomFieldDocs = R.printRsQTyName RR.phantomData <$ TD.collectPhantomTyArgs iTyDefs mn tyN fieldTys tyArgs
      mayBoxedFields = zip vals $ TD.isRecursive iTyDefs mn tyN <$> fieldTys

  fieldDocs <- for mayBoxedFields (printMaybeBoxed pkgs iTyDefs)

  return $ ctorDoc <> encloseSep lparen rparen comma (fieldDocs <> phantomFieldDocs)

printMaybeBoxed :: MonadPrint m => R.PkgMap -> PC.TyDefs -> (LV.ValueE, Bool) -> m (Doc ann)
printMaybeBoxed pkgs iTyDefs (val, False) = toOwned <$> printValueE pkgs iTyDefs val
printMaybeBoxed pkgs iTyDefs (val, True) = do
  valDoc <- toOwned <$> printValueE pkgs iTyDefs val
  return $ R.printRsQValName RR.boxNew <> parens valDoc

printTupleE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> LV.ValueE -> m (Doc ann)
printTupleE pkgs iTyDefs l r = do
  lDoc <- printValueE pkgs iTyDefs l
  rDoc <- printValueE pkgs iTyDefs r
  return $ parens (lDoc <> comma <+> rDoc)

printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE = return . fromStr . dquotes . pretty

printCaseTextE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE pkgs iTyDefs txtVal cases otherCase = do
  caseValDoc <- printValueE pkgs iTyDefs txtVal
  caseDocs <-
    for
      cases
      ( \case
          (LV.TextE caseTxt, bodyVal) -> do
            conditionDoc <- printTextE caseTxt
            bodyDoc <- printValueE pkgs iTyDefs bodyVal
            return $ group $ conditionDoc <+> "=>" <+> bodyDoc
          (_wrongCaseVal, _) -> throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )
  otherDoc <- printOtherCase pkgs iTyDefs otherCase
  return $ "ma" <> align ("tch" <+> caseValDoc <+> braces (line <> vsep (punctuate comma (caseDocs <> [otherDoc]))))

{- | Print a reference

 HACK(szg251)
 To help Rust type inference, we inject type information for a few references.
 In case of trait methods and associated functions, such as `toPlutusData`, `fromPlutusData`,
 `toJson`, `fromJson` we call the reference as a trait method on the target type:
 ```rs
 <TargetType>::to_plutus_data(...)
 ```
 In case of functions with a generic argument, such as `jsonConstructor`, we print the target type in a turbofish syntax:
 ```rs
 lbr_prelude::json::json_constructr::<TargetType>::toPlutusData(...)
 ```
-}
printRefE :: MonadPrint m => R.PkgMap -> LV.Ref -> m (Doc ann)
printRefE pkgs ref = do
  qvn <- LV.resolveRef ref
  case ref of
    (argTy : _, builtin)
      | builtin == "toPlutusData"
          || builtin == "fromPlutusData"
          || builtin == "toJson"
          || builtin == "fromJson" -> do
          lamTyDoc <- printLamTy pkgs argTy
          methodDoc <- R.printRsValName . R.qualifiedEntity <$> LV.importValue qvn
          return $ angles lamTyDoc <> R.doubleColon <> methodDoc
      | builtin == "jsonConstructor" -> do
          lamTyDoc <- printLamTy pkgs argTy
          methodDoc <- R.printRsQValName <$> LV.importValue qvn
          return $ methodDoc <> R.doubleColon <> langle <> lamTyDoc <> rangle
    _other -> R.printRsQValName <$> LV.importValue qvn

printLamTy :: MonadPrint m => R.PkgMap -> LT.Ty -> m (Doc ann)
printLamTy pkgs (LT.TyRef tyRef) = return $ R.printTyRef pkgs tyRef
printLamTy _ (LT.TyVar tyVar) = return $ R.printTyVar tyVar
printLamTy pkgs (LT.TyApp ty gens _) = do
  tyDoc <- printLamTy pkgs ty
  gensDoc <- for gens (printLamTy pkgs)
  return $ tyDoc <> R.encloseGenerics gensDoc
printLamTy _ _ = throwInternalError "Unexpected LamTy expression."

printValueE :: MonadPrint m => R.PkgMap -> PC.TyDefs -> LV.ValueE -> m (Doc ann)
printValueE _ _ (LV.VarE v) = return $ pretty v
printValueE pkgs _ (LV.RefE ref) = printRefE pkgs ref
printValueE pkgs iTyDefs (LV.LamE lamVal) = printLamE pkgs iTyDefs lamVal
printValueE pkgs iTyDefs (LV.AppE funVal argVal) = printAppE pkgs iTyDefs funVal argVal
printValueE pkgs iTyDefs (LV.CaseE sumTy caseVal ctorCont) = printCaseE pkgs iTyDefs sumTy caseVal ctorCont
printValueE pkgs iTyDefs (LV.CtorE qctor prodVals) = printCtorE pkgs iTyDefs qctor prodVals
printValueE pkgs iTyDefs (LV.RecordE qrec vals) = printRecordE pkgs iTyDefs qrec vals
printValueE pkgs iTyDefs (LV.FieldE fieldName recVal) = printFieldE pkgs iTyDefs fieldName recVal
printValueE pkgs iTyDefs (LV.ProductE qprod vals) = printProductE pkgs iTyDefs qprod vals
printValueE pkgs iTyDefs (LV.LetE prodTy prodVal letCont) = printLetE pkgs iTyDefs prodTy prodVal letCont
printValueE _ _ (LV.IntE i) = return $ pretty i
printValueE pkgs iTyDefs (LV.CaseIntE intVal cases otherCase) = printCaseIntE pkgs iTyDefs intVal cases otherCase
printValueE pkgs iTyDefs (LV.ListE vals) = printNewListE pkgs iTyDefs vals
printValueE pkgs iTyDefs (LV.CaseListE listVal cases otherCase) = printCaseListE pkgs iTyDefs listVal cases otherCase
printValueE _ _ (LV.TextE txt) = printTextE txt
printValueE pkgs iTyDefs (LV.CaseTextE txtVal cases otherCase) = printCaseTextE pkgs iTyDefs txtVal cases otherCase
printValueE pkgs iTyDefs (LV.TupleE l r) = printTupleE pkgs iTyDefs l r
printValueE _ _ (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called " <> err

-- | HACK(szg251): This is a hack, to help Rust figure out types of closures (lambda expressions)
printInstance :: MonadPrint m => R.PkgMap -> [R.QTyName] -> PC.TyDefs -> LV.ValueE -> m (Doc ann)
printInstance pkgs [] iTyDefs lamVal = printValueE pkgs iTyDefs lamVal
printInstance pkgs argTys iTyDefs (LV.LamE lamVal) = printInstanceLamE pkgs argTys iTyDefs lamVal
printInstance _ _ _ _ = throwInternalError "LamE expression expected with predefined argument types"

-- | HACK(szg251): This is a hack, to help Rust figure out types of closures (lambda expressions)
printInstanceLamE :: MonadPrint m => R.PkgMap -> [R.QTyName] -> PC.TyDefs -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printInstanceLamE _ [] _ _ = throwInternalError "LamE expression expected with predefined argument types"
printInstanceLamE pkgs (argTy : argTys) iTyDefs lamVal = do
  arg <- LV.freshArg
  let body = lamVal arg
  bodyDoc <- printInstance pkgs argTys iTyDefs body
  argDoc <- printValueE pkgs iTyDefs arg
  let argTy' = "&'a" <+> R.printRsQTyName argTy

  return $ R.printRsQValName RR.boxNew <> parens ("move" <+> pipe <> argDoc <> colon <+> argTy' <> pipe <+> braces (space <> group bodyDoc))
