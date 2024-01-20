module LambdaBuffers.Codegen.Typescript.Print.TyDef (printTyDef) where

import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (printTyName)
import LambdaBuffers.Codegen.Typescript.Print.Ty (printTyAbs)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, group, vsep, (<+>))

{- | Prints the LambdaBuffers type definition into a Typescript type definition.

```lbf
sum SumFoo a b = Baz Text a | Bar Integer b
prod ProdFoo a = Text a
prod ProdBar a = a
record RecFoo a = { bar : Text, baz : a}
record RecBar a = { bar : a}
```

translates to something like the following:

```ts
export type SumFoo<$a, $b> =
    | { name: 'Baz'
      , fields: [Text, $a]
      }
    | { name: 'Bar'
      , fields: [Integer, $b]
      }
export const SumFoo: unique symbol = Symbol('SumFoo');

export type ProdFoo<$a> = [Text, $a]
export const ProdFoo: unique symbol = Symbol('ProdFoo');

// Note products with exactly one projection don't have a list around it
export type ProdBar<$a> = $a
export const ProdBar: unique symbol = Symbol('ProdBar')

export type RecFoo<$a> = { bar : Text, baz : $a }
export const RecFoo: unique symbol = Symbol('RecFoo');

export type RecBar<$a> = { bar : $a }
export const RecBar: unique symbol = Symbol('RecBar');
```
-}
printTyDef :: MonadPrint m => Ts.PkgMap -> PC.TyDef -> m (Doc ann)
printTyDef pkgMap (PC.TyDef tyN tyabs _) = do
  (absDoc, symbolDoc) <- printTyAbs pkgMap tyN tyabs
  return $
    vsep
      [ group $ "export" <+> tyDefDecl <+> printTyName tyN <> absDoc
      , group $ "export" <+> "const" <+> printTyName tyN <+> symbolDoc
      ]

tyDefDecl :: Doc ann
tyDefDecl = "type"
