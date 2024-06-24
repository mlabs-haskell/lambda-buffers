module LambdaBuffers.Codegen.Haskell.Backend.Plutarch.TyDef (printTyDef, printTyInner) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Haskell.Backend (MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Backend.Plutarch.Refs qualified as PlRefs
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HaskellNative
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as HaskellNative
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, encloseSep, equals, group, hardline, hsep, parens, pipe, sep, space, vsep, (<+>))

{- | Prints the type definition in Plutarch.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
......................................
prod FooProd a b = (Maybe a) b
..............................
record FooRecord a b = { a: Maybe a, b: b }
...........................................
opaque FooOpaque a  b
.....................
prod FooProdUnit a = (Maybe a)
..............................
record FooRecUnit a = { a: Maybe a }
....................................
```

translates to

```haskell
data FooSum (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooSum'Foo (Plutarch.Term s (Plutarch.Builtin.PAsData (PMaybe a))) | FooSum'Bar (Plutarch.Term s (Plutarch.Builtin.PAsData b))
..........................................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
data FooProd (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooProd (Plutarch.Term s (Plutarch.Builtin.PAsData (PMaybe a))) (Plutarch.Term s (Plutarch.Builtin.PAsData b))
...........................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
data FooRecord (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooRecord (Plutarch.Term s (Plutarch.Builtin.PAsData (PMaybe a))) (Plutarch.Term s (Plutarch.Builtin.PAsData b))
...............................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
.................................................
newtype FooProdUnit (a :: Plutarch.PType) (s :: Plutarch.S) = FooProdUnit (Plutarch.Term s (Plutarch.Builtin.PAsData (PMaybe a)))
.................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
newtype FooRecUnit (a :: Plutarch.PType) (s :: Plutarch.S) = FooRecUnit (Plutarch.Term s (Plutarch.Builtin.PAsData (PMaybe a)))
...............................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
```

And signals the following imports:

```haskell
import qualified Plutarch
import qualified Plutarch.Builtin
import qualified Plutarch.Show
import qualified GHC.Generics
import qualified Some.Configured.Opaque
```

NOTE(bladyjoker): The full qualification is omitted in the following docstrings for brevity, as are deriving statements.
-}
printTyDef :: MonadHaskellBackend t m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  Print.importType PlRefs.termQTyName
  Print.importType PlRefs.scopeQTyName
  Print.importType PlRefs.ptypeQTyName
  Print.importType PlRefs.pasDataQTyName
  drvGenericDoc <- printDerivingGeneric
  drvShowDoc <- printDerivingShow
  (kw, absDoc) <- printTyAbs tyN tyabs
  let tyDefDoc = group $ printTyDefKw kw <+> HaskellNative.printTyName tyN <+> absDoc
  if kw == HaskellNative.SynonymTyDef
    then return tyDefDoc
    else return $ tyDefDoc <> hardline <> space <> space <> align (vsep [drvGenericDoc, drvShowDoc])

printDerivingShow :: MonadHaskellBackend t m => m (Doc ann)
printDerivingShow = do
  Print.importClass PlRefs.showQClassName
  return $ "deriving anyclass" <+> HaskellNative.printHsQClassName PlRefs.showQClassName

printDerivingGeneric :: MonadHaskellBackend t m => m (Doc ann)
printDerivingGeneric = do
  Print.importClass PlRefs.genericQClassName
  return $ "deriving stock" <+> HaskellNative.printHsQClassName PlRefs.genericQClassName

printTyDefKw :: HaskellNative.TyDefKw -> Doc ann
printTyDefKw HaskellNative.DataTyDef = "data"
printTyDefKw HaskellNative.NewtypeTyDef = "newtype"
printTyDefKw HaskellNative.SynonymTyDef = "type"

{- | Prints the type abstraction.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
           ...........................
prod FooProd a b = (Maybe a) b
             .................
record FooRecord a b = { a: Maybe a, b: b }
                 ..........................
opaque FooOpaque a b
                 ...
prod FooProdUnit a = (Maybe a)
                 .............
record FooRecUnit a = { a: Maybe a }
                  ..................
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
            ...............................................................................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
             ...............................................................................................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
               .................................................................................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
               ..................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))
                    .................................................................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
                   ................................................................
```

NOTE(bladyjoker): We don't print the `s` Scope type argument/variable and others because `The type synonym ‘Prelude.Plutarch.Integer’ should have 1 argument, but has been given none` in `Term s Prelude.Plutarch.Integer`. We also don't print other args because it's either all args or none.
-}
printTyAbs :: MonadHaskellBackend t m => PC.TyName -> PC.TyAbs -> m (HaskellNative.TyDefKw, Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  let scopeArgDoc :: Doc ann
      scopeArgDoc = parens ("s" <+> "::" <+> HaskellNative.printHsQTyName PlRefs.scopeQTyName)
      argsDoc =
        if kw == HaskellNative.SynonymTyDef
          then mempty
          else hsep $ (printTyArg <$> toList args) <> [scopeArgDoc]
  return (kw, group $ argsDoc <+> equals <+> align bodyDoc)

{- | Prints the type body.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                 ......................
prod FooProd a b = (Maybe a) b
                   ...........
record FooRecord a b = { a: Maybe a, b: b }
                       ....................
opaque FooOpaque a b
prod FooProdUnit a = (Maybe a)
                     .........
record FooRecUnit a = { a: Maybe a }
                      ..............
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
                                                 ..........................................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
                                                  ..........................................................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
                                                    ............................................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
                 ................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))
                                            .........................................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
                                           ........................................
```

TODO(bladyjoker): Revisit empty records and prods.
-}
printTyBody :: MonadHaskellBackend t m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (HaskellNative.TyDefKw, Doc ann)
printTyBody tyN _ (PC.SumI s) = (HaskellNative.DataTyDef,) <$> printSum tyN s
printTyBody tyN _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return (HaskellNative.DataTyDef, HaskellNative.printMkCtor tyN)
  [_] -> do
    prodDoc <- printProd p
    return (HaskellNative.NewtypeTyDef, HaskellNative.printMkCtor tyN <+> prodDoc)
  _other -> do
    prodDoc <- printProd p
    return (HaskellNative.DataTyDef, HaskellNative.printMkCtor tyN <+> prodDoc)
printTyBody tyN _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return (HaskellNative.DataTyDef, HaskellNative.printMkCtor tyN)
  [_] -> do
    recDoc <- printRec r
    return (HaskellNative.NewtypeTyDef, HaskellNative.printMkCtor tyN <+> recDoc)
  _other -> do
    recDoc <- printRec r
    return (HaskellNative.DataTyDef, HaskellNative.printMkCtor tyN <+> recDoc)
printTyBody tyN _args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> Print.throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (HaskellNative.SynonymTyDef, HaskellNative.printHsQTyName hqtyn)

{- | Prints the type (abstraction) arguments.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
           . .
prod FooProd a b = (Maybe a) b
             . .
record FooRecord a b = { a: Maybe a, b: b }
                 . .
opaque FooOpaque a b
                 . .
prod FooProdUnit a = (Maybe a)
                 .
record FooRecUnit a = { a: Maybe a }
                  .
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
            ............ ............
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
             ............ ............
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
               ............ ............
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))
                    ............
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
                   ............
```
-}
printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = parens (HaskellNative.printVarName vn <+> "::" <+> HaskellNative.printHsQTyName PlRefs.ptypeQTyName)

{- | Prints the sum body.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                 .....................
prod FooProd a b = (Maybe a) b

record FooRecord a b = { a: Maybe a, b: b }

opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)

record FooRecUnit a = { a: Maybe a }
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
                                                 ..........................................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
```
-}
printSum :: MonadHaskellBackend t m => PC.TyName -> PC.Sum -> m (Doc ann)
printSum tyN (PC.Sum ctors _) = do
  ctorDocs <- traverse (printCtor tyN) (toList ctors)
  return $
    group $
      if null ctors
        then mempty
        else align $ encloseSep mempty mempty (space <> pipe <> space) ctorDocs -- TODO(bladyjoker): Make it align on the ConstrName.

{- | Prints the sum constructor.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                 .............   .....
prod FooProd a b = (Maybe a) b

record FooRecord a b = { a: Maybe a, b: b }

opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)

record FooRecUnit a = { a: Maybe a }
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
                                                 ........................................   ...............................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
```
-}
printCtor :: MonadHaskellBackend t m => PC.TyName -> PC.Constructor -> m (Doc ann)
printCtor tyN (PC.Constructor ctorName prod) = do
  let ctorNDoc = HaskellNative.printCtorName tyN ctorName
  prodDoc <- printProd prod
  return $ group $ ctorNDoc <+> prodDoc -- TODO(bladyjoker): Adds extra space when empty.

{- | Prints the record body.

NOTE(bladyjoker): This prints as a product body, keeping the order of fields as defined at source.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b

prod FooProd a b = (Maybe a) b

record FooRecord a b = { a: Maybe a, b: b }
                       ....................
opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)

record FooRecUnit a = { a: Maybe a }
                      ..............
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))

data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
                                                              ..................................................
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
                                                      .............................
```
-}
printRec :: MonadHaskellBackend t m => PC.Record -> m (Doc ann)
printRec (PC.Record fields si) = printProd (PC.Product (PC.fieldTy <$> toList fields) si)

{- | Prints the product body.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                     .........       .
prod FooProd a b = (Maybe a) b
                   ...........
record FooRecord a b = { a: Maybe a, b: b }

opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)
                     .........
record FooRecUnit a = { a: Maybe a }
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PAsData (PMaybe a))) | FooSum'Bar (Term s (PAsData b))
                                                            .............................              ....................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PAsData (PMaybe a))) (Term s (PAsData b))
                                                          ..................................................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PAsData (PMaybe a))) (Term s (PAsData b))

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PAsData (PMaybe a)))
                                                        .............................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PAsData (PMaybe a)))
```
-}
printProd :: MonadHaskellBackend t m => PC.Product -> m (Doc ann)
printProd (PC.Product fields _) = do
  fieldDocs <-
    traverse
      ( \f -> do
          fieldDoc <- printTyInner f
          return $ parens (HaskellNative.printHsQTyName PlRefs.termQTyName <+> "s" <+> parens (HaskellNative.printHsQTyName PlRefs.pasDataQTyName <+> fieldDoc))
      )
      fields
  return $
    if null fields
      then mempty
      else align $ sep fieldDocs

printTyInner :: MonadHaskellBackend t m => PC.Ty -> m (Doc ann)
printTyInner = HaskellNative.printTyInner
