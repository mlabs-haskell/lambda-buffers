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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.Typescript.Print.Names (printCtorName, printFieldName, printTsQValName)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, dquotes, encloseSep, equals, group, indent, lbrace, lbracket, lparen, parens, rbrace, rbracket, rparen, vsep, (<+>))
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
      --
      -- Note(jaredponn): having a unique '_instanceTyIx' immediately implies
      -- that this only supports single parameter type classes.
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
                          -- If there's a unique element in the fields,
                          -- recall that it's not wrapped in a tuple
                          -- list.
                          -- See
                          -- 'LambdaBuffers.Codegen.Typescript.Print.Ty.printProd'
                          -- for details.
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
    --  2. do the case expression as @if ... else if ... else if ... blah blah blah@
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

printLamE :: forall ann m. MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  -- Something like
  -- @
  --    f a b c d
  -- @
  -- becomes
  -- @
  --    f(a,b,c,d)
  -- @
  -- which follows the intuition from most imperative languages i.e., the idea
  -- of returning repeated function closures is a silly e.g. @f(a)(b)(c)(d)@ is
  -- silly.
  let uncurrySpine :: LV.ValueE -> m ([LV.ValueE], LV.ValueE)
      uncurrySpine = \case
        LV.LamE f -> do
          arg <- LV.freshArg
          let body = f arg
          result <- uncurrySpine body

          return $ first (arg :) result
        t -> return ([], t)

  (args, body) <- uncurrySpine $ LV.LamE lamVal

  argsDoc <- for args printValueE
  bodyDoc <- printValueE body

  return $
    parens $
      vsep
        [ encloseSep lparen rparen comma argsDoc <+> "=>" <+> lbrace
        , indent 2 ("return" <+> bodyDoc)
        , rbrace
        ]

printAppE :: forall m ann. MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  -- Again, like 'printLamE', we uncurry the spine of applications.
  let uncurrySpine :: LV.ValueE -> m (LV.ValueE, [LV.ValueE])
      uncurrySpine = \case
        LV.AppE l r -> second (r :) <$> uncurrySpine l
        t -> return (t, [])

  (f, args) <- second reverse <$> uncurrySpine (LV.AppE funVal argVal)

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
                      -- If there's a unique element in the fields,
                      -- recall that it's not wrapped in a tuple
                      -- list.
                      -- See
                      -- 'LambdaBuffers.Codegen.Typescript.Print.Ty.printProd'
                      -- for details.
                      [singleField] ->
                        [ "let" <+> singleField <+> equals <+> letRHSDoc
                        -- WARN(jaredponn): recall that products are "raw
                        -- tuples", so it's wrong to access `.field`
                        -- <> ".fields"
                        ]
                      _ ->
                        zipWith
                          ( \i argDoc ->
                              "let"
                                <+> argDoc
                                <+> equals
                                <+> letRHSDoc
                                <>
                                -- WARN(jaredponn): see above warning about
                                -- these raw tuples.
                                -- ".fields" <>
                                lbracket
                                <> pretty (show i)
                                <> rbracket
                          )
                          [0 :: Int ..]
                          argDocs
                      ++ ["return" <+> bodyDoc]
                  )
            , rbrace
            ]
      )
      <> parens letValDoc

printOtherCase :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printOtherCase otherCase = do
  arg <- LV.freshArg
  argDoc <- printValueE arg
  bodyDoc <- printValueE $ otherCase arg
  return $ group $ argDoc <+> "->" <+> bodyDoc

printCaseIntE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  -- The value we are casing on
  caseOnIntValDoc <- printValueE caseIntVal

  -- The argument for what we are casing on
  caseOnArg <- LV.freshArg
  caseOnArgDoc <- printValueE caseOnArg

  -- Compile each of the branches
  casesDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE conditionVal
          bodyDoc <- printValueE bodyVal
          return
            ( conditionDoc
            , vsep
                [ lbrace
                , indent 2 $ "return" <+> bodyDoc
                , rbrace
                ]
            )
      )

  otherDoc <-
    fmap ("return" <+>) $
      printValueE $
        otherCase caseOnArg

  return $
    -- TODO(jaredponn): Do we need this outermost 'parens'?
    parens $
      parens
        ( parens caseOnArgDoc
            <+> "=>"
            <+> vsep
              [ lbrace
              , indent 2 $
                  vsep $
                    case casesDocs of
                      [] ->
                        [otherDoc]
                      (num1, body1) : ts ->
                        [ "if" <+> parens (caseOnArgDoc <+> "===" <+> num1)
                        , body1
                        ]
                          ++ concatMap
                            ( \(num, body) ->
                                [ "else" <+> "if" <+> parens (num <+> "===" <+> caseOnArgDoc)
                                , body
                                ]
                            )
                            ts
                          ++ [ "else"
                             , lbrace
                             , indent 2 otherDoc
                             , rbrace
                             ]
              , rbrace
              ]
        )
        <> parens caseOnIntValDoc

printListE :: MonadPrint m => [LV.ValueE] -> m (Doc ann)
printListE vals = do
  valDocs <- printValueE `traverse` vals
  return $ align (encloseSep lbracket rbracket comma valDocs)

printCaseListE :: MonadPrint m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE caseListVal cases otherCase = do
  -- The value we are casing on
  caseListValDoc <- printValueE caseListVal

  -- The argument for what we are casing on
  caseOnArg <- LV.freshArg
  caseOnArgDoc <- printValueE caseOnArg

  -- Compile each of the branches
  casesDocs <-
    for
      cases
      ( \(listLength, bodyVal) -> do
          listVars <- replicateM listLength LV.freshArg
          listVarsDocs <- for listVars printValueE
          bodyDoc <- printValueE (bodyVal listVars)
          return
            ( pretty listLength
            , vsep
                [ lbrace
                , indent 2 $
                    vsep $
                      zipWith
                        ( \i argDoc ->
                            "let" <+> argDoc <+> equals <+> caseOnArgDoc <> lbracket <> pretty (show i) <> rbracket <> "!"
                        )
                        [0 :: Int ..]
                        listVarsDocs
                        ++ ["return" <+> bodyDoc]
                , rbrace
                ]
            )
      )

  otherDoc <-
    fmap ("return" <+>) $
      printValueE $
        otherCase caseOnArg

  -- Build the lambda which has the statements
  return $
    parens
      ( parens caseOnArgDoc
          <+> "=>"
          <+> vsep
            [ lbrace
            , indent 2 $
                vsep $
                  case casesDocs of
                    [] ->
                      [otherDoc]
                    (num1, body1) : ts ->
                      [ "if" <+> parens (caseOnArgDoc <> ".length" <+> "===" <+> num1)
                      , body1
                      ]
                        ++ concatMap
                          ( \(num, body) ->
                              [ "else" <+> "if" <+> parens (caseOnArgDoc <> ".length" <+> "===" <+> num)
                              , body
                              ]
                          )
                          ts
                        ++ [ "else"
                           , lbrace
                           , indent 2 otherDoc
                           , rbrace
                           ]
            , rbrace
            ]
      )
      <> parens caseListValDoc

printCtorE :: MonadPrint m => LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE ((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals printValueE
  let ctorNDoc = printCtorName (withInfo tyN) (withInfo ctorN)
  case prodDocs of
    [] ->
      return $
        vsep
          [ lbrace
          , indent 2 $ "name" <+> colon <+> pretty '\'' <> ctorNDoc <> pretty '\''
          , rbrace
          ]
    [singleDoc] ->
      return $
        vsep
          [ lbrace
          , indent 2 $
              vsep
                [ "fields" <+> colon <+> singleDoc <> comma
                , "name" <+> colon <+> pretty '\'' <> ctorNDoc <> pretty '\''
                ]
          , rbrace
          ]
    _ ->
      return $
        vsep
          [ lbrace
          , indent 2 $
              vsep
                [ "fields" <+> colon <+> encloseSep lbracket rbracket comma prodDocs <> comma
                , "name" <+> colon <+> pretty '\'' <> ctorNDoc <> pretty '\''
                ]
          , rbrace
          ]

printRecordE :: MonadPrint m => LV.QRecord -> [(LV.Field, LV.ValueE)] -> m (Doc ann)
printRecordE ((_, tyN), _) vals = do
  fieldDocs <- for vals $
    \((fieldN, _), val) -> case printFieldName (withInfo tyN) (withInfo fieldN) of
      Nothing -> throwInternalError "Failed printing field name"
      Just fieldNDoc -> do
        valDoc <- printValueE val
        return $ group $ fieldNDoc <+> colon <+> valDoc
  -- Note(jaredponn): Again, we have no need for the constructor name because
  -- languages like Haskell have:
  -- @
  -- MyRecord { field1 = a, field2 = b }
  -- @
  -- whereas in TS we remove the constructor altogether i.e., we just have
  -- @
  -- { field1 = a, field2 = b }
  -- @
  -- so the following is unnecessary:
  -- > let ctorDoc = printMkCtor (withInfo tyN)
  return $ align (encloseSep lbrace rbrace comma fieldDocs)

printProductE :: MonadPrint m => LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE ((_, _tyN), _) vals = do
  -- If there's a unique element in the fields,
  -- recall that it's not wrapped in a tuple
  -- list.
  -- See
  -- 'LambdaBuffers.Codegen.Typescript.Print.Ty.printProd'
  -- for details.
  fieldDocs <- for vals printValueE
  -- Note(jaredponn): normally, we'd have to write down the constructor name
  -- e.g.
  -- @MyProd a b c@
  -- but we recall that TS products are literally anonymous tuples, so this
  -- would be just
  -- @[a,b,c]@
  --
  -- In other words, the following makes no sense at all:
  -- @
  -- let ctorDoc = printMkCtor (withInfo tyN)
  -- return $ ctorDoc <+> align (hsep fieldDocs)
  -- @
  case fieldDocs of
    [f] -> return f
    fs -> return (encloseSep lbracket rbracket comma fs)

-- We only use bigints and they are suffixed by a @n@
printIntE :: MonadPrint m => Int -> m (Doc ann)
printIntE i =
  return $ pretty i <> "n"

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
      --      say
      --      > dictCMyType
      --
      --      - Types of the form @MyType a1 .. aN@ where a1,..,aN are types
      --      have instance dictionary as say
      --      > dictCMyType(dictCa1, ... , dictCaN)
      --      where we may inductively assume that each a1,..,aN have
      --      instance dictionary dictCa1,..,dictCaN
      --
      -- WARNING(jaredponn): this makes the assumption that _all_ instances are
      -- Haskell2010 type classes i.e., every instance is of the form
      --
      -- > instance C (T u1 .. uk)
      --
      -- where u1,..,uk are _simple_ type variables and distinct, and @T@ is a type
      -- constructor.
      --
      -- See 4.3.2 of https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3
      --
      -- In particular, this will silently generate invalid code if the
      -- instance is not valid Haskell2010.
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
  return $ align (encloseSep lbracket rbracket comma [lDoc, rDoc])

printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE = return . dquotes . pretty

printCaseTextE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE txtVal cases otherCase = do
  -- The value that we are casing on
  caseValDoc <- printValueE txtVal

  -- The argument for what we are casing on
  caseOnArg <- LV.freshArg
  caseOnArgDoc <- printValueE caseOnArg

  -- Compile the branches
  casesDocs <-
    for
      cases
      ( \case
          (LV.TextE caseTxt, bodyVal) -> do
            conditionDoc <- printTextE caseTxt
            bodyDoc <- printValueE bodyVal
            return
              ( conditionDoc
              , vsep
                  [ lbrace
                  , indent 2 $ vsep ["return" <+> bodyDoc]
                  , rbrace
                  ]
              )
          (_wrongCaseVal, _) ->
            throwInternalError "Expected a TextE as the case value but got something else (TODO(bladyjoker): Print got)"
      )

  otherDoc <- printOtherCase otherCase

  -- build the lambda which has the statements
  return $
    parens $
      parens caseOnArgDoc
        <+> "=>"
        <+> vsep
          [ lbrace
          , indent 2 $
              vsep $
                case casesDocs of
                  [] ->
                    [otherDoc]
                  (txt1, body1) : ts ->
                    [ "if" <+> parens (caseOnArgDoc <+> "===" <+> txt1)
                    , body1
                    ]
                      ++ concatMap
                        ( \(txt, body) ->
                            [ "else" <+> "if" <+> parens (caseOnArgDoc <+> "===" <+> txt)
                            , body
                            ]
                        )
                        ts
                      ++ [ "else"
                         , lbrace
                         , indent 2 otherDoc
                         , rbrace
                         ]
          , rbrace
          ]
        <> parens caseValDoc

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
