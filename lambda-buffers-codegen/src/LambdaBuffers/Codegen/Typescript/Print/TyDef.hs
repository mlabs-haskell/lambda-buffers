module LambdaBuffers.Codegen.Typescript.Print.TyDef (printTyDef) where

import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (printTyName)
import LambdaBuffers.Codegen.Typescript.Print.Ty (printTyAbs)
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, group, (<+>))

{- | Prints the LambdaBuffers type definition into a Typescript type definition.

```lbf
sum SumFoo a b = Baz Text a | Bar Integer b
prod ProdFoo a = Text a
prod ProdBar a = a
record RecFoo a = { bar : Text, baz : a}
record RecBar a = { bar : a}
```

translates to

```purescript
TODO
```
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  absDoc <- printTyAbs tyN tyabs
  return $ group $ "export" <+> tyDefDecl <+> printTyName tyN <> absDoc

tyDefDecl :: Doc ann
tyDefDecl = "type"
