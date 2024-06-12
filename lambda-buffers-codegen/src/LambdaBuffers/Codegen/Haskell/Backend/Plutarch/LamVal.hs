module LambdaBuffers.Codegen.Haskell.Backend.Plutarch.LamVal (printValueE) where

import Control.Lens ((&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.List qualified as List
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, backslash, dquotes, group, hardline, hsep, line, parens, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

throwInternalError :: MonadPrint m => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.Plutarch] " <> Text.pack msg

type MonadPrint m = LV.MonadPrint m HsSyntax.QValName

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id

-- * Plutarch references *

plamRef :: HsSyntax.QValName
plamRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "plam")

pappRef :: HsSyntax.QValName
pappRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "#")

pconRef :: HsSyntax.QValName
pconRef = (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch", HsSyntax.MkValueName "pcon")

pmatchRef :: HsSyntax.QValName
pmatchRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "pmatch")

pnilRef :: HsSyntax.QValName
pnilRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "PNil")

pconsRef :: HsSyntax.QValName
pconsRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "PCons")

pconstantRef :: HsSyntax.QValName
pconstantRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "pconstant")

pifRef :: HsSyntax.QValName
pifRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "pif")

peqRef :: HsSyntax.QValName
peqRef = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "#==")

-- * LamVal interpretation *

{- | `printLamE lamVal` prints a `lambda abstraction` expression.

```haskell
printLamE (\x -> <expression over x>)
```

translates to Plutarch

```haskell
plam (\x -> <expression over x>)
```
-}
printLamE :: MonadPrint m => (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printLamE lamVal = do
  arg <- LV.freshArg
  bodyDoc <- printValueE (lamVal arg)
  argDoc <- printValueE arg
  plamDoc <- HsSyntax.printHsQValName <$> LV.importValue plamRef
  return $ plamDoc <+> parens (backslash <> argDoc <+> "->" <+> group bodyDoc)

{- | `printAppE funVal argVal` prints a `lambda application` expression.

```haskell
printAppE funVal argVal
```

translates to Plutarch

```haskell
(#) (funVal) (argVal)
```
-}
printAppE :: MonadPrint m => LV.ValueE -> LV.ValueE -> m (Doc ann)
printAppE funVal argVal = do
  funDoc <- printValueE funVal
  argDoc <- printValueE argVal
  pappDoc <- HsSyntax.printHsQValName <$> LV.importValue pappRef
  return $ pappDoc <+> parens funDoc <+> group (parens argDoc)

{- | `printCtorE qctor prodVals` prints a sum type constructor of type `qctor` with the body type of `prodVals` expression.

```lbf
sum Foo a b = Bar a b | Baz b
```

```haskell
printCtorE ("Foo", ("Bar", ["a", "b"])) [<x : a>, <y : b>]
```

translates to Plutarch

```haskell
pcon (Foo'Bar (x) (y))
```

TODO(bladyjoker): Add import for the `Foo'Bar` constructor value reference.
-}
printCtorE :: MonadPrint m => LV.QCtor -> [LV.ValueE] -> m (Doc ann)
printCtorE _qctor@((_, tyN), (ctorN, _)) prodVals = do
  prodDocs <- for prodVals (fmap parens . printValueE)
  let ctorNDoc = HsSyntax.printCtorName (withInfo tyN) (withInfo ctorN)
  pconDoc <- HsSyntax.printHsQValName <$> LV.importValue pconRef
  if null prodDocs
    then return $ pconDoc <+> ctorNDoc
    else return $ pconDoc <+> parens (ctorNDoc <+> align (hsep prodDocs))

{- | `printCaseE qsum caseVal ctorCont` prints a pattern match on a `caseVal` value of `qsum` type, and supplies the result to `ctorCont` continuation function.

```lbf
sum Foo a b = Bar a b | Baz b
```

```haskell
printCaseE ("Foo",
             [
             ("Bar", ["a", "b"]),
             ("Baz", ["b"])
             ])
           (\case
             (("Bar", ["a", "b"]), [<x : a>, <y : b>]) -> <expression on Bar and x y>
             (("Baz", ["b"]), [<x : b>]) -> <expression on Baz and x>
           )
```

translates to Plutarch

```haskell
pmatch foo (\x -> case x of
                    Foo'Bar x1 x2 -> <expression on Bar and x1 x2>
                    Foo'Baz x3 -> <expression on Baz and x3>
           )
```
-}
printCaseE :: MonadPrint m => LV.QSum -> LV.ValueE -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> m (Doc ann)
printCaseE _qsum@(qtyN, sumTy) caseVal ctorCont = do
  caseValDoc <- printValueE caseVal
  ctorCaseDocs <-
    vsep
      <$> for
        (OMap.assocs sumTy)
        ( \(cn, ty) -> case ty of -- TODO(bladyjoker): Cleanup by refactoring LT.Ty.
            LT.TyProduct fields _ -> printCtorCase qtyN ctorCont (cn, fields)
            _other -> throwInternalError "Got a non-product in Sum."
        )
  pmatchDoc <- HsSyntax.printHsQValName <$> LV.importValue pmatchRef
  pmatchContArgDoc <- LV.freshArg >>= printValueE
  let casesDoc = "ca" <> align ("se" <+> pmatchContArgDoc <+> "of" <> line <> ctorCaseDocs)
  return $ pmatchDoc <+> caseValDoc <+> parens (backslash <> pmatchContArgDoc <+> "->" <+> casesDoc)

printCtorCase :: MonadPrint m => PC.QTyName -> ((LV.Ctor, [LV.ValueE]) -> LV.ValueE) -> LV.Ctor -> m (Doc ann)
printCtorCase (_, tyn) ctorCont ctor@(ctorN, fields) = do
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let body = ctorCont (ctor, args)
  bodyDoc <- printValueE body
  let ctorNameDoc = HsSyntax.printCtorName (withInfo tyn) . withInfo $ ctorN
  if null argDocs
    then return $ group $ ctorNameDoc <+> "->" <+> group bodyDoc
    else return $ group $ ctorNameDoc <+> hsep argDocs <+> "->" <+> group bodyDoc

{- | `printProductE qprod vals` prints a value of Product type `qprod` with the body type of `vals`.

```lbf
prod Foo a b = a b
```

```haskell
printProductE ("Foo", ["a", "b"]) [<x : a>, <y : b>]
```

translates to Plutarch

```haskell
pcon (Foo (x) (y))
```

TODO(bladyjoker): Add Product constructor import.
-}
printProductE :: MonadPrint m => LV.QProduct -> [LV.ValueE] -> m (Doc ann)
printProductE ((_, tyN), _) vals = do
  fieldDocs <- for vals (fmap parens . printValueE)
  let ctorDoc = HsSyntax.printMkCtor (withInfo tyN)
  pconDoc <- HsSyntax.printHsQValName <$> LV.importValue pconRef
  return $ pconDoc <+> parens (ctorDoc <+> align (hsep fieldDocs))

{- | `printLetE qprod prodVal prodCont` prints a product pattern match a `prodVal` value of product type `qprod` and supplies the result to `prodCont`

NOTE(bladyjoker): 'let' seems to be a misnomer, as this is product deconstruction rather than just expression binding to a variable. Plutarch makes that distinction on `plet` vs `pmatch`

```lbf
prod Foo a b = a b
```

```haskell
printLetE `prod Foo a b = a b` `foo` (\[x, y] -> <expression on x and y>)
```

translates to Plutarch

```haskell
pmatch foo (\(Foo x y) -> <expression on x and y>)
```
-}
printLetE :: MonadPrint m => LV.QProduct -> LV.ValueE -> ([LV.ValueE] -> LV.ValueE) -> m (Doc ann)
printLetE ((_, tyN), fields) prodVal letCont = do
  prodValDoc <- printValueE prodVal
  args <- for fields (const LV.freshArg)
  argDocs <- for args printValueE
  let bodyVal = letCont args
  bodyDoc <- printValueE bodyVal
  let prodCtorDoc = HsSyntax.printMkCtor (withInfo tyN)
  pmatchDoc <- HsSyntax.printHsQValName <$> LV.importValue pmatchRef
  return $ pmatchDoc <+> prodValDoc <+> parens (backslash <> parens (prodCtorDoc <+> hsep argDocs) <+> "->" <+> bodyDoc)

{- | `printListE vals` prints a list expression.

```haskell
printListE [`x`, `y`]
```

translates to Plutarch

```haskell
pcon (PCons x (PCons y PNil))
```
-}
printListE :: MonadPrint m => [LV.ValueE] -> m (Doc ann)
printListE [] = do
  pconDoc <- HsSyntax.printHsQValName <$> LV.importValue pconRef
  pnilDoc <- HsSyntax.printHsQValName <$> LV.importValue pnilRef
  return $ pconDoc <+> pnilDoc
printListE (val : vals) = do
  valDoc <- printValueE val
  valsDoc <- printListE vals
  pconDoc <- HsSyntax.printHsQValName <$> LV.importValue pconRef
  pconsDoc <- HsSyntax.printHsQValName <$> LV.importValue pconsRef
  return $ pconDoc <+> parens (pconsDoc <+> parens valDoc <+> parens valsDoc)

{- | `printCaseListE vals` prints a list pattern match expression.

NOTE(bladyjoker): This is too complicated to even talk about.

I'm basically unfolding `pmatch` with `PCon` and `PNil` and calling the `cases` when the 'length' is just right.

Consider the following 'case' expression:

```haskell
case xs of
  [] -> a
  [x1,x2] -> b x1 x2
  [x1,x2,x3] -> c x1 x2 x3
  _ -> d xs
```

The naive implementation would do, in the case of xs being `[1,2,3,4]`:

1. does [1,2,3,4] match []?
3. does [1,2,3,4] match [x1, x2]?
4. does [1,2,3,4] match [x1, x2, x3]?
5. bind [1,2,3,4] to _ and call `d xs`

You're already seeing the issue, work is duplicated.

Instead what I'm trying to print:

```haskell
case xs of
  [] -> a
  h1:t1 -> case t1 of
    [] -> d xs -- OTHER
    h2:t2 -> case t2 of
      [] -> b h1 h2
      h3:t3 -> case t3 of
        [] -> c h1 h2 h4
        h4:t4 -> d xs -- OTHER
```
-}
printCaseListE :: MonadPrint m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseListE xs cases otherCase = do
  let maxLength = maximum $ fst <$> cases
  otherCaseDoc <- printValueE (otherCase xs)
  printCaseListE' xs cases otherCaseDoc 0 maxLength []

printCaseListE' :: MonadPrint m => LV.ValueE -> [(Int, [LV.ValueE] -> LV.ValueE)] -> Doc ann -> Int -> Int -> [LV.ValueE] -> m (Doc ann)
printCaseListE' _xs _cases otherCaseDoc currentLength maxLength _args | currentLength > maxLength = return otherCaseDoc
printCaseListE' xs cases otherCaseDoc currentLength maxLength args = do
  pnilRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pnilRef
  pconsRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pconsRef
  pmatchRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pmatchRef
  xsDoc <- printValueE xs
  xsMatched <- LV.freshArg
  xsMatchedDoc <- printValueE xsMatched
  headArg <- LV.freshArg
  headArgDoc <- printValueE headArg
  tailArg <- LV.freshArg
  tailArgDoc <- printValueE tailArg
  otherOrCaseDoc <- maybe (return otherCaseDoc) (\c -> printValueE $ c (reverse args)) (List.lookup currentLength cases)
  restDoc <- printCaseListE' tailArg cases otherCaseDoc (currentLength + 1) maxLength (headArg : args)
  return $
    pmatchRefDoc
      <+> xsDoc
      <+> parens
        ( backslash
            <> xsMatchedDoc
            <+> "->"
            <+> "case"
            <+> xsMatchedDoc
            <+> "of"
            <> align
              ( hardline
                  <> vsep
                    [ pnilRefDoc <+> "->" <+> otherOrCaseDoc
                    , pconsRefDoc <+> headArgDoc <+> tailArgDoc <+> "->" <+> restDoc
                    ]
              )
        )

{- | `printIntE i` prints an integer literal expression.

```haskell
printIntE 1
printIntE -1
```

translates to Plutarch

```haskell
pconstant 1
pconstant (-1)
```
-}
printIntE :: MonadPrint m => Int -> m (Doc ann)
printIntE i = do
  pconstantRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pconstantRef
  return $ pconstantRefDoc <+> if i < 0 then parens (pretty i) else pretty i

{- | `printCaseIntE intVal cases otherCase` prints an integer case expression.

```haskell
printCaseIntE `x` [(0, <A>), (123, <B>)] (\other -> <C>)
```

translates to Plutarch

```haskell
pif ((#==) (x) (pconstant 0)) <A> (pif ((#==) (x) (pconstant 123)) <B> <C>)
```
-}
printCaseIntE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal [] otherCase = printValueE (otherCase caseIntVal) -- TODO(bladyjoker): Why is this a function and not just a ValueE?
printCaseIntE caseIntVal ((iVal, bodyVal) : cases) otherCase = do
  pifRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pifRef
  peqRefDoc <- HsSyntax.printHsQValName <$> LV.importValue peqRef
  caseIntValDoc <- printValueE caseIntVal
  iValDoc <- printValueE iVal -- TODO(bladyjoker): Why am I handing a ValueE and not Int?
  bodyValDoc <- printValueE bodyVal
  elseDoc <- printCaseIntE caseIntVal cases otherCase
  return $ pifRefDoc <+> parens (peqRefDoc <+> parens caseIntValDoc <+> parens iValDoc) <+> parens bodyValDoc <+> parens elseDoc

{- | `printTextE t` prints a text literal expression.

```haskell
printTextE "Dražen Popović"
```

translates to Plutarch

```haskell
pconstant "Dražen Popović"
```
-}
printTextE :: MonadPrint m => Text.Text -> m (Doc ann)
printTextE t = do
  pconstantRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pconstantRef
  return $ pconstantRefDoc <+> dquotes (pretty t)

{- | `printCaseTextE tVal cases otherCase` prints a text case expression.

```haskell
printCaseTextE `x` [("a", <A>), ("b", <B>)] (\other -> <C>)
```

translates to Plutarch

```haskell
pif ((#==) (x) (pconstant "a")) <A> (pif ((#==) (x) (pconstant "b")) <B> <C>)
```
-}
printCaseTextE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseTextE caseTxtVal [] otherCase = printValueE (otherCase caseTxtVal) -- TODO(bladyjoker): Why is this a function and not just a ValueE?
printCaseTextE caseTxtVal ((txtVal, bodyVal) : cases) otherCase = do
  pifRefDoc <- HsSyntax.printHsQValName <$> LV.importValue pifRef
  peqRefDoc <- HsSyntax.printHsQValName <$> LV.importValue peqRef
  caseTxtValDoc <- printValueE caseTxtVal
  txtValDoc <- printValueE txtVal -- TODO(bladyjoker): Why am I handing a ValueE and not a Text?
  bodyValDoc <- printValueE bodyVal
  elseDoc <- printCaseIntE caseTxtVal cases otherCase
  return $ pifRefDoc <+> parens (peqRefDoc <+> parens caseTxtValDoc <+> parens txtValDoc) <+> parens bodyValDoc <+> parens elseDoc

printRefE :: MonadPrint m => LV.Ref -> m (Doc ann)
printRefE ref = do
  qvn <- LV.resolveRef ref
  HsSyntax.printHsQValName <$> LV.importValue qvn

printValueE :: MonadPrint m => LV.ValueE -> m (Doc ann)
printValueE (LV.VarE v) = return $ pretty v
printValueE (LV.RefE ref) = printRefE ref
printValueE (LV.LamE lamVal) = printLamE lamVal
printValueE (LV.AppE funVal argVal) = printAppE funVal argVal
printValueE (LV.CaseE sumTy caseVal ctorCont) = printCaseE sumTy caseVal ctorCont
printValueE (LV.CtorE qctor prodVals) = printCtorE qctor prodVals
printValueE (LV.ProductE qprod vals) = printProductE qprod vals
printValueE (LV.LetE prodTy prodVal letCont) = printLetE prodTy prodVal letCont
printValueE (LV.IntE i) = printIntE i
printValueE (LV.CaseIntE intVal cases otherCase) = printCaseIntE intVal cases otherCase
printValueE (LV.ListE vals) = printListE vals
printValueE (LV.CaseListE listVal cases otherCase) = printCaseListE listVal cases otherCase
printValueE (LV.TextE txt) = printTextE txt
printValueE (LV.CaseTextE txtVal cases otherCase) = printCaseTextE txtVal cases otherCase
printValueE (LV.TupleE _l _r) = throwInternalError "LamVal tuple literal expression is not supported for Plutarch (yet)"
printValueE (LV.RecordE _qrec _vals) = throwInternalError "LamVal record literal expression is not supported for Plutarch"
printValueE (LV.FieldE _fieldName _recVal) = throwInternalError "LamVal record field accessor is not supported for Plutarch"
printValueE (LV.ErrorE err) = throwInternalError $ "LamVal error builtin was called with: " <> err
