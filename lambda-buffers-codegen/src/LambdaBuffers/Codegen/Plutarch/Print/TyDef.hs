module LambdaBuffers.Codegen.Plutarch.Print.TyDef (printTyDef, printTyInner) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Haskell.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsPrint
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Plutarch.Print.Syntax qualified as PlSyntax
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, dot, encloseSep, equals, group, hsep, parens, pipe, sep, space, (<+>))

{- | Prints the type definition.

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
data FooSum (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooSum'Foo (Plutarch.Term s (PMaybe a)) | FooSum'Bar (Plutarch.Term s b)
....................................................................................................................................................
data FooProd (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooProd (Plutarch.Term s (PMaybe a)) (Plutarch.Term s b)
.....................................................................................................................................
data FooRecord (a :: Plutarch.PType) (b :: Plutarch.PType) (s :: Plutarch.S) = FooRecord (Plutarch.Term s (PMaybe a)) (Plutarch.Term s b)
.........................................................................................................................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
.................................................
newtype FooProdUnit (a :: Plutarch.PType) (s :: Plutarch.S) = FooProdUnit (Plutarch.Term s (PMaybe a))
......................................................................................................
newtype FooRecUnit (a :: Plutarch.PType) (s :: Plutarch.S) = FooRecUnit (Plutarch.Term s (PMaybe a))
....................................................................................................
```

And signals the following imports:

```haskell
import qualified Plutarch
import qualified Some.Configured.Opaque
```

NOTE(bladyjoker): The full qualification is omitted in the following docstrings for brevity.
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  Print.importType termType
  Print.importType scopeType
  Print.importType ptypeType
  (kw, absDoc) <- printTyAbs tyN tyabs
  return $ group $ printTyDefKw kw <+> HsSyntax.printTyName tyN <+> absDoc

printTyDefKw :: HsSyntax.TyDefKw -> Doc ann
printTyDefKw HsSyntax.DataTyDef = "data"
printTyDefKw HsSyntax.NewtypeTyDef = "newtype"
printTyDefKw HsSyntax.SynonymTyDef = "type"

-- Plutarch internal type imports (Term, PType, S).

termType :: HsSyntax.QTyName
termType = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "Term")

scopeType :: HsSyntax.QTyName
scopeType = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "S")

ptypeType :: HsSyntax.QTyName
ptypeType = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "PType")

-- Plutarch derived classes (Generic, PShow).

_showClass :: HsSyntax.QClassName
_showClass = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Show", HsSyntax.MkClassName "PShow")

_printDerivingShow :: MonadPrint m => m (Doc ann)
_printDerivingShow = do
  Print.importClass _showClass
  return $ "deriving anyclass" <+> HsSyntax.printHsQClassName _showClass

_genericClass :: HsSyntax.QClassName
_genericClass = (HsSyntax.MkCabalPackageName "base", HsSyntax.MkModuleName "GHC.Generics", HsSyntax.MkClassName "Generic")

_printDerivingGeneric :: MonadPrint m => m (Doc ann)
_printDerivingGeneric = do
  Print.importClass _genericClass
  return $ "deriving stock" <+> HsSyntax.printHsQClassName _genericClass

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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
            ...........................................................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
             ...........................................................................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
               .............................................................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
               ..................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                    .......................................................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                   ......................................................
```

NOTE(bladyjoker): We don't print the `s` Scope type argument/variable and others because `The type synonym ‘Prelude.Plutarch.Integer’ should have 1 argument, but has been given none` in `Term s Prelude.Plutarch.Integer`. We also don't print other args because it's either all args or none.
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (HsSyntax.TyDefKw, Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  let scopeArgDoc :: Doc ann
      scopeArgDoc = parens ("s" <+> "::" <+> HsSyntax.printHsQTyName scopeType)
      argsDoc =
        if kw == HsPrint.SynonymTyDef
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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                 ......................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
                                                  ......................................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
                                                    ........................................
type FooOpaque = Some.Configured.Opaque.FooOpaque
                 ................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                            ...............................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                           ..............................
```

TODO(bladyjoker): Revisit empty records and prods.
-}
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (HsSyntax.TyDefKw, Doc ann)
printTyBody tyN _ (PC.SumI s) = (HsSyntax.DataTyDef,) <$> printSum tyN s
printTyBody tyN _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return (HsSyntax.DataTyDef, HsSyntax.printMkCtor tyN)
  [_] -> return (HsSyntax.NewtypeTyDef, HsSyntax.printMkCtor tyN <+> printProd p)
  _ -> return (HsSyntax.DataTyDef, HsSyntax.printMkCtor tyN <+> printProd p)
printTyBody tyN _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return (HsSyntax.DataTyDef, HsSyntax.printMkCtor tyN)
  [_] -> return (HsSyntax.NewtypeTyDef, HsSyntax.printMkCtor tyN <+> printRec r)
  _ -> return (HsSyntax.DataTyDef, HsSyntax.printMkCtor tyN <+> printRec r)
printTyBody tyN _args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> Print.throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (HsSyntax.SynonymTyDef, HsSyntax.printHsQTyName hqtyn)

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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
            ............ ............
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
             ............ ............
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
               ............ ............
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                    ............
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                   ............
```
-}
printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = parens (HsSyntax.printVarName vn <+> "::" <+> HsSyntax.printHsQTyName ptypeType)

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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                 ......................................................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
```
-}
printSum :: MonadPrint m => PC.TyName -> PC.Sum -> m (Doc ann)
printSum tyN (PC.Sum ctors _) = do
  let ctorDocs = printCtor tyN <$> toList ctors
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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                 ..............................   .....................
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
```
-}
printCtor :: PC.TyName -> PC.Constructor -> Doc ann
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = HsSyntax.printCtorName tyN ctorName
      prodDoc = printProd prod
   in group $ ctorNDoc <+> prodDoc -- TODO(bladyjoker): Adds extra space when empty.

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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)

data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)

data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
                                                              ..............................
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                      ...................
```
-}
printRec :: PC.Record -> Doc ann
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
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                            ...................              ..........
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
                                                          ..............................
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)

type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                        ...................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
```
-}
printProd :: PC.Product -> Doc ann
printProd (PC.Product fields _) = do
  if null fields
    then mempty
    else align $ sep ((\f -> parens (HsSyntax.printHsQTyName termType <+> "s" <+> printTyInner f)) <$> fields)

printTyInner :: PC.Ty -> Doc ann
printTyInner (PC.TyVarI v) = printTyVar v
printTyInner (PC.TyRefI r) = printTyRef r
printTyInner (PC.TyAppI a) = printTyAppInner a

{- | Prints the 'inner' type application.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                      .......
prod FooProd a b = (Maybe a) b
                    .......
record FooRecord a b = { a: Maybe a, b: b }
                            .......
opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)
                      .......
record FooRecUnit a = { a: Maybe a }
                           .......
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                                    ..........
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
                                                                  ..........
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
                                                                       ........
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                                 ........
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                               ........
```
-}
printTyAppInner :: PC.TyApp -> Doc ann
printTyAppInner (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ parens $ fDoc <+> align (sep argsDoc)

{- | Prints the type reference.

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                      .....
prod FooProd a b = (Maybe a) b
                    .....
record FooRecord a b = { a: Maybe a, b: b }
                            .....
opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)
                      .....
record FooRecUnit a = { a: Maybe a }
                           .....
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                                     ......
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
                                                                   ......
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
                                                                       ......
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                                 ......
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                               ......
```
-}
printTyRef :: PC.TyRef -> Doc ann
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ HsSyntax.printTyName tn
printTyRef (PC.ForeignI fr) = let (_, HsSyntax.MkModuleName hmn, HsSyntax.MkTyName htn) = PlSyntax.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

{- | Prints the type variable (remember args are different to vars).

```lbf
sum FooSum a b = Foo (Maybe a) | Bar b
                            .        .
prod FooProd a b = (Maybe a) b
                          .  .
record FooRecord a b = { a: Maybe a, b: b }
                                  .     .
opaque FooOpaque a b

prod FooProdUnit a = (Maybe a)
                            .
record FooRecUnit a = { a: Maybe a }
                                 .
```

translates to

```haskell
data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Foo (Term s (PMaybe a)) | FooSum'Bar (Term s b)
                                                                            .                        .
data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s (PMaybe a)) (Term s b)
                                                                          .           .
data FooRecord (a :: PType) (b :: PType) (s :: S) = FooRecord (Term s (PMaybe a)) (Term s b)
                                                                              .           .
type FooOpaque = Some.Configured.Opaque.FooOpaque

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                                        .
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                                      .

```
-}
printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar vn) = HsSyntax.printVarName vn
