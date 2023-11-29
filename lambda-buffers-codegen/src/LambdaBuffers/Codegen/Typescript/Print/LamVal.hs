module LambdaBuffers.Codegen.Typescript.Print.LamVal (
  printValueE,
  Builtin (OverloadedBuiltin, Builtin),
  qualifiedValName,
  instanceTyIx,
  dotMethodName,
) where

import Control.Arrow (first, second)
import Control.Lens (ix, (&), (.~), (^?), _1)
import Control.Lens.TH (makeLenses)
import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Typescript.Print.Names (printCtorName, printFieldName, printMkCtor, printTsQValName)

-- import LambdaBuffers.Codegen.Typescript.Syntax (normalValName)

import Data.Text (Text)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, dquotes, encloseSep, equals, group, hsep, indent, lbrace, lbracket, line, lparen, parens, rbrace, rbracket, rparen, space, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

{- | 'Builtin' is the result of a symbol table lookup for things like
 'LambdaBuffers.Codegen.Typescript.Print.Derive.lvEqBuiltins', etc.
-}
data Builtin
  = OverloadedBuiltin
      { _qualifiedValName :: Ts.QValName
      -- ^ the qualified name of the dictionary for the typeclass
      , _instanceTyIx :: Int
      -- ^ index (starting at 0) of the type in the 'LV.RefE' which is
      -- the instance type e.g.
      -- Given
      -- > class MyClass a where
      -- >    myclass :: b -> a -> String
      -- > instance MyClass Int where
      -- >    myclass :: b -> a -> String
      -- >    myclass = ...
      -- The method @myclass@ in the instance has 'RefE' as something
      -- (loosely) like
      -- > ([TyVar "b", TyRef Int], "myclass")
      --
      -- so the '_instanceTyIx' would be 1 in this case.
      , _dotMethodName :: Text
      -- ^ the method name of the dictionary prefixed with a dot.
      }
  | Builtin
      { _qualifiedValName :: Ts.QValName
      -- ^ the qualified name of the method
      }
  deriving stock (Eq, Ord)

$(makeLenses ''Builtin)

type MonadPrint m = LV.MonadPrint m Builtin

throwInternalError :: MonadPrint m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Typescript.Print.LamVal] " <> Text.pack msg

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

-- tuple :: Ts.QValName
-- tuple = normalValName "tuple" "Data.Tuple" "Tuple"

-- caseInt :: Ts.QValName
-- caseInt = normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseInt"

-- fromInt :: Ts.QValName
-- fromInt = normalValName "js-bigints" "JS.BigInt" "fromInt"

printCaseE :: forall m ann. MonadPrint m => (PC.QTyName, LV.Sum) -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE (qtyN, sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE caseVal
  -- Compile the branches
  caseOnArg <- LV.freshArg
  caseOnArgDoc <- printValueE caseOnArg

  let printCtorCase :: (PC.InfoLess PC.ConstrName, LT.Ty) -> m (Doc ann, Doc ann)
      printCtorCase (cn, ty) = case ty of
        LT.TyProduct fields _ -> do
          -- Get the the constructor name
          let ctorNameDoc = printCtorName (withInfo $ snd qtyN) . withInfo $ cn

          -- Create fresh variables for the arguments
          args <- for fields (const LV.freshArg)
          argDocs <- for args printValueE

          -- Print / compile the body
          let body = ctorCont ((cn, fields), args)

          bodyDoc <- printValueE body

          return
            ( -- Create each branch of the cases by returning a tuple of the
              -- constructor name, and the body of the branch
              -- > ( 'ConstructorNameK'
              -- > , {
              -- >        let fresh0 = caseOnDoc.fields[0]
              -- >        let fresh1 = caseOnDoc.fields[1]
              -- >        ...
              -- >        let freshN = caseOnDoc.fields[N]
              -- >        <compile body>
              -- >   }
              -- > )
              --
              -- Edge cases: if there is a unique "freshI", then we just have
              --
              -- > fresh0 = caseOnDoc.fields
              pretty '\'' <> ctorNameDoc <> pretty '\''
            , vsep
                [ lbrace
                , indent 2 $
                    vsep
                      ( case argDocs of
                          [] -> []
                          -- if there's a unique element in the fields,
                          -- recall that it's not wrapped in a tuple
                          -- list.
                          [singleField] -> ["let" <+> singleField <+> equals <+> caseOnArgDoc <> ".fields"]
                          _ ->
                            zipWith
                              ( \i argDoc ->
                                  "let" <+> argDoc <+> equals <+> caseOnArgDoc <> ".fields" <> lbracket <> pretty (show i) <> rbracket
                              )
                              [0 :: Int ..]
                              argDocs
                          ++ ["return" <+> bodyDoc]
                      )
                , rbrace
                ]
            )
        _ -> throwInternalError "Got a non-product in Sum."

  casesDocs <- case OMap.assocs sumTy of
    t : ts -> do
      (cn1, cnBody1) <- printCtorCase t
      cnsCnBodies <- for ts printCtorCase

      return $
        ("if" <+> parens (caseOnArgDoc <> dot <> "name" <+> "===" <+> cn1) <> cnBody1)
          : map
            ( \(cn, cnBody) ->
                "else" <+> "if" <+> parens (caseOnArgDoc <> dot <> "name" <+> "===" <+> cn) <> cnBody
            )
            cnsCnBodies
    _ -> throwInternalError "Empty case not supported"

  return $
    -- Loosely, we
    --  1. create a lambda; then
    --  2. do the case expression as @if else blah blah blah@
    parens
      ( parens caseOnArgDoc
          <+> "=>"
          <+> vsep
            [ lbrace
            , indent 2 $
                vsep
                  casesDocs
            , rbrace
            ]
      )
      <> parens caseValDoc

{- |
 Remark:
  Chains of nested abstractions are uncurried.
-}
printLamE :: forall ann m. MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  let resolveChain :: LV.ValueE -> m ([LV.ValueE], LV.ValueE)
      resolveChain = \case
        LV.LamE f -> do
          arg <- LV.freshArg
          let body = f arg
          result <- resolveChain body

          return $ first (arg :) result
        t -> return ([], t)

  (args, body) <- resolveChain $ LV.LamE lamVal

  argsDoc <- for args printValueE
  bodyDoc <- printValueE body

  return $
    parens $
      vsep
        [ encloseSep lparen rparen comma argsDoc <+> "=>" <+> lbrace
        , indent 2 ("return" <+> bodyDoc)
        , rbrace
        ]

-- | Left associative chains of apps are uncurried
printAppE :: forall m ann. MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  let resolveChain :: LV.ValueE -> m (LV.ValueE, [LV.ValueE])
      resolveChain = \case
        LV.AppE l r -> second (r :) <$> resolveChain l
        t -> return (t, [])

  (f, args) <- second reverse <$> resolveChain (LV.AppE funVal argVal)

  fDoc <- printValueE f
  argsDoc <- for args printValueE

  return $
    fDoc <> align (encloseSep lparen rparen comma argsDoc)

printFieldE :: MonadPrint m => LV.QField -> LV.ValueE -> m (Doc ann)
printFieldE ((_, tyN), fieldN) recVal = do
  recDoc <- printValueE recVal
  case printFieldName (withInfo tyN) (withInfo fieldN) of
    Nothing -> throwInternalError $ "Failed print a `FieldName` in Typescript implementation printer " <> show fieldN
    Just fnDoc -> do
      return $ parens recDoc <> dot <> fnDoc

printLetE :: MonadPrint m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, _tyN), fields) prodVal letCont = do
  letRHS <- LV.freshArg
  letRHSDoc <- printValueE letRHS

  letValDoc <- printValueE prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  -- let prodCtorDoc = printMkCtor (withInfo tyN)
  return $
    parens
      ( parens letRHSDoc
          <+> "=>"
          <+> vsep
            [ lbrace
            , indent 2 $
                vsep
                  ( case argDocs of
                      [] -> []
                      -- if there's a unique element in the fields,
                      -- recall that it's not wrapped in a tuple
                      -- list.
                      [singleField] -> ["let" <+> singleField <+> equals <+> letRHSDoc <> ".fields"]
                      _ ->
                        zipWith
                          ( \i argDoc ->
                              "let" <+> argDoc <+> equals <+> letRHSDoc <> ".fields" <> lbracket <> pretty (show i) <> rbracket
                          )
                          [0 :: Int ..]
                          argDocs
                      ++ ["return" <+> bodyDoc]
                  )
            , rbrace
            ]
      )
      <> parens letValDoc

-- "let" <+> prodCtorDoc <+> hsep argDocs <+> equals <+> letValDoc <+> "in" <+> bodyDoc

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
          -- tupleDoc <- printTsQValName <$> LV.importValue tuple
          tupleDoc <- undefined
          return $ group $ tupleDoc <+> conditionDoc <+> parens bodyDoc
      )
  let casesDoc = lbracket <> align (encloseSep mempty mempty (comma <> space) casesDocs <> rbracket)
  otherDoc <- printValueE $ LV.LamE otherCase
  caseIntDoc <- undefined
  -- caseIntDoc <- printTsQValName <$> LV.importValue caseInt
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
printIntE i =
  return $ pretty i <> "n"

-- LV.importValue fromInt >>= (\fromIntDoc -> return $ parens $ fromIntDoc <+> pretty i) . printTsQValName

printRefE :: forall m ann. MonadPrint m => LV.Ref -> m (Doc ann)
printRefE ref@(refTys, _) = do
  refBuiltin <- LV.resolveRef ref
  _ <- LV.importValue refBuiltin
  case refBuiltin of
    Builtin qvn -> return $ printTsQValName qvn
    OverloadedBuiltin _dict instanceTyIndex dotMethod -> do
      -- First, we grab the instance type e.g.
      -- > instance MyClass (SomeType a b c d)
      --                    ^~~~~~~~~~~~~~~~~~ this type
      instanceTy <- case refTys ^? ix instanceTyIndex of
        Just instanceTy -> return instanceTy
        _ -> throwInternalError "Overloaded instance type doesn't exist"

      -- Then, we do the type directed translation.
      --
      -- Type directed translation to resolve the instances.
      -- Assumptions:
      --  - All instances are in scope.
      --  - Instances may be derived "structurally" for class @C@ in the
      --  following inductive sense:
      --
      --      - Types of the form @MyType@ simply have instance dictionary as
      --      > dictCMyType
      --
      --      - Types of the form @MyType a1 .. aN@ where a1,..,aN are types
      --      have instance dictionary as
      --      > dictCMyType(dictCa1, ... , dictCaN)
      --      where we may inductively assume that each a1,..,aN have
      --      instance dictionary dictCa1,..,dictCaN

      let
        -- type directed translation_s_
        tdts :: [LT.Ty] -> m (Doc ann)
        tdts [] = return mempty
        tdts list = do
          tysDoc <- for list tdt
          return $ align $ encloseSep lparen rparen comma tysDoc

        -- type directed translation
        tdt :: LT.Ty -> m (Doc ann)
        tdt (LT.TyApp ty tys _) = do
          builtin <- LV.resolveRef (ref & _1 . ix instanceTyIndex .~ ty)
          _ <- LV.importValue builtin
          case builtin of
            OverloadedBuiltin d i _
              | i == instanceTyIndex -> (printTsQValName d <>) <$> tdts tys
              | otherwise -> throwInternalError "Overloaded methods should be overloaded on the same type"
            Builtin _ -> throwInternalError "Expected a an OverloadedBuiltin when resolving a type class"
        tdt ty@(LT.TyRef _tyRef) = do
          -- essentially similar to the previous case, except we don't recurse
          builtin <- LV.resolveRef (ref & _1 . ix instanceTyIndex .~ ty)
          _ <- LV.importValue builtin
          case builtin of
            OverloadedBuiltin d i _
              | i == instanceTyIndex -> return $ printTsQValName d
              | otherwise -> throwInternalError "Overloaded methods should be overloaded on the same type"
            Builtin _ -> throwInternalError "Expected a an OverloadedBuiltin when resolving a type class"
        tdt ty@(LT.TyVar _tyRef) = do
          -- duplicated code from the previous case
          builtin <- LV.resolveRef (ref & _1 . ix instanceTyIndex .~ ty)
          _ <- LV.importValue builtin
          case builtin of
            Builtin _ -> throwInternalError "Expected a an OverloadedBuiltin when resolving a type class"
            OverloadedBuiltin d i _
              | i == instanceTyIndex -> return $ printTsQValName d
              | otherwise -> throwInternalError "Overloaded methods should be overloaded on the same type"
        tdt _ = throwInternalError "Unexpected type when resolving a type class"
      resolvedTypeClass <- tdt instanceTy
      return $ resolvedTypeClass <> pretty dotMethod

printTupleE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printTupleE l r = do
  lDoc <- printValueE l
  rDoc <- printValueE r
  -- tupleDoc <- printTsQValName <$> LV.importValue tuple
  tupleDoc <- undefined
  return $ parens (tupleDoc <+> parens lDoc <+> parens rDoc)

printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE = return . dquotes . pretty

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
            return $ group $ conditionDoc <+> "->" <+> bodyDoc
          (_wrongCaseVal, _) -> throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )
  otherDoc <- printOtherCase otherCase
  return $ "ca" <> align ("se" <+> caseValDoc <+> "of" <> line <> vsep (caseDocs <> [otherDoc]))

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
printValueE (LV.TextE txt) = printTextE txt
printValueE (LV.CaseTextE txtVal cases otherCase) = printCaseTextE txtVal cases otherCase
printValueE (LV.TupleE l r) = printTupleE l r
printValueE (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called " <> err
