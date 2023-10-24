module LambdaBuffers.Codegen.Plutarch.Print.TyDef (printTyDef, printTyInner) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Haskell.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Names qualified as HsNames
import LambdaBuffers.Codegen.Haskell.Syntax (TyDefKw (DataTyDef, NewtypeTyDef, SynonymTyDef))
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, dot, encloseSep, equals, group, hardline, parens, pipe, sep, space, vsep, (<+>))

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
data FooSum (a :: Plutarch.Internal.PType) (b :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = FooSum'Foo (Plutarch.Internal.Term s (PMaybe a)) | FooSum'Bar (Plutarch.Internal.Term s b)
.................................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................

data FooProd (a :: Plutarch.Internal.PType) (b :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = FooProd (Plutarch.Internal.Term s (PMaybe a)) (Plutarch.Internal.Term s b)
..................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................

data FooRecord (a :: Plutarch.Internal.PType) (b :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = FooRecord (Plutarch.Internal.Term s (PMaybe a)) (Plutarch.Internal.Term s b)
......................................................................................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................

type FooOpaque (a :: Plutarch.Internal.PType) (b :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = Some.Configured.Opaque.FooOpaque a b s
................................................................................................................................................

newtype FooProdUnit (a :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = FooProdUnit (Plutarch.Internal.Term s (PMaybe a))
...............................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................

newtype FooRecUnit (a :: Plutarch.Internal.PType) (s :: Plutarch.Internal.S) = FooRecUnit (Plutarch.Internal.Term s (PMaybe a))
...............................................................................................................................
  deriving stock GHC.Generics.Generic
  ...................................
  deriving anyclass Plutarch.Show.PShow
  .....................................
```

And signals the following imports:

```haskell
import qualified Plutarch.Internal
import qualified GHC.Generics
import qualified Plutarch.Show
import qualified Some.Configured.Opaque
```

NOTE(bladyjoker): The full qualification and deriving statements are omitted in the following docstrings for brevity.
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  Print.importType termType
  Print.importType scopeType
  Print.importType ptypeType
  (kw, absDoc) <- printTyAbs tyN tyabs
  if kw /= SynonymTyDef
    then do
      drvGenericDoc <- printDerivingGeneric
      drvShowDoc <- printDerivingShow
      return $ group $ printTyDefKw kw <+> HsNames.printTyName tyN <+> absDoc <> hardline <> vsep [drvGenericDoc, drvShowDoc]
    else return $ group $ printTyDefKw kw <+> HsNames.printTyName tyN <+> absDoc

printTyDefKw :: TyDefKw -> Doc ann
printTyDefKw DataTyDef = "data"
printTyDefKw NewtypeTyDef = "newtype"
printTyDefKw SynonymTyDef = "type"

-- Plutarch internal type imports (Term, PType, S).
-- FIX(bladyjoker): Use H.QTyName and invent importType

termType :: H.QTyName
termType = (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Internal", H.MkTyName "Term")

scopeType :: H.QTyName
scopeType = (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Internal", H.MkTyName "S")

ptypeType :: H.QTyName
ptypeType = (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Internal", H.MkTyName "PType")

-- Plutarch derived classes (Generic, PShow).

showClass :: H.QClassName
showClass = (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Show", H.MkClassName "PShow")

printDerivingShow :: MonadPrint m => m (Doc ann)
printDerivingShow = do
  Print.importClass showClass
  return $ "deriving anyclass" <+> HsNames.printHsQClassName showClass

genericClass :: H.QClassName
genericClass = (H.MkCabalPackageName "base", H.MkModuleName "GHC.Generics", H.MkClassName "Generic")

printDerivingGeneric :: MonadPrint m => m (Doc ann)
printDerivingGeneric = do
  Print.importClass genericClass
  return $ "deriving stock" <+> HsNames.printHsQClassName genericClass

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s
               ...........................................................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                    .......................................................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                   ......................................................
```
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (TyDefKw, Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc = if OMap.empty == args then mempty else encloseSep mempty space space (printTyArg <$> toList args)
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  return (kw, group $ argsDoc <+> parens ("s" <+> "::" <+> HsNames.printHsQTyName scopeType) <> align (equals <+> bodyDoc))

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s
                                                    ......................................
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                            ...............................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                           ..............................
```

TODO(bladyjoker): Revisit empty records and prods.
-}
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (TyDefKw, Doc ann)
printTyBody tyN _ (PC.SumI s) = (DataTyDef,) <$> printSum tyN s
printTyBody tyN _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return (DataTyDef, HsNames.printMkCtor tyN)
  [_] -> return (NewtypeTyDef, HsNames.printMkCtor tyN <+> printProd p)
  _ -> return (DataTyDef, HsNames.printMkCtor tyN <+> printProd p)
printTyBody tyN _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return (DataTyDef, HsNames.printMkCtor tyN)
  [_] -> return (NewtypeTyDef, HsNames.printMkCtor tyN <+> printRec r)
  _ -> return (DataTyDef, HsNames.printMkCtor tyN <+> printRec r)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> Print.throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (SynonymTyDef, HsNames.printHsQTyName hqtyn <> space <> sep ((HsNames.printVarName . view #argName <$> args) ++ ["s"]))

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s
               ............ ............
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                    ............
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                   ............
```
-}
printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = parens (HsNames.printVarName vn <+> "::" <+> HsNames.printHsQTyName ptypeType)

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

type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

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

type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))

newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
```
-}
printCtor :: PC.TyName -> PC.Constructor -> Doc ann
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = HsNames.printCtorName tyN ctorName
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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

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

type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                        ...................
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
```
-}
printProd :: PC.Product -> Doc ann
printProd (PC.Product fields _) = do
  if null fields
    then mempty
    else align $ sep ((\f -> parens (HsNames.printHsQTyName termType <+> "s" <+> printTyInner f)) <$> fields)

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s

newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                                 ......
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                               ......
```
-}
printTyRef :: PC.TyRef -> Doc ann
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ HsNames.printTyName tn
printTyRef (PC.ForeignI fr) = let (_, H.MkModuleName hmn, H.MkTyName htn) = H.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

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
type FooOpaque (a :: PType) (b :: PType) (s :: S) = Some.Configured.Opaque.FooOpaque a b s
                                                                                     . .
newtype FooProdUnit (a :: PType) (s :: S) = FooProdUnit (Term s (PMaybe a))
                                                                        .
newtype FooRecUnit (a :: PType) (s :: S) = FooRecUnit (Term s (PMaybe a))
                                                                      .

```
-}
printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar vn) = HsNames.printVarName vn
