module LambdaBuffers.Codegen.Purescript.Print.TyDef (printTyDef) where

import LambdaBuffers.Codegen.Purescript.Print.InstanceDef (printGenericDerive, printNewtypeDerive, printShowInstance)
import LambdaBuffers.Codegen.Purescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Purescript.Print.Names (printTyName)
import LambdaBuffers.Codegen.Purescript.Print.Ty (printTyAbs)
import LambdaBuffers.Codegen.Purescript.Syntax (TyDefKw (DataTyDef, NewtypeTyDef, SynonymTyDef))
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, group, vsep, (<+>))

{- | Prints the LambdaBuffers type definition into a Purescript type definition.

```lbf
sum SumFoo a b = Baz Text a | Bar Integer b
prod ProdFoo a = Text a
prod ProdBar a = a
record RecFoo a = { bar : Text, baz : a}
record RecBar a = { bar : a}
```

translates to

```purescript
data SumFoo a b = SumFoo'Baz LambdaBuffers.Prelude.Text a
                   | SumFoo'Bar LambdaBuffers.Prelude.Integer b

derive instance Data.Generic.Rep.Generic (SumFoo a b) _
instance (Data.Show.Show a,Data.Show.Show b) => Data.Show.Show (SumFoo a
                                                                       b) where
  show = Data.Show.Generic.genericShow

data ProdFoo a = ProdFoo LambdaBuffers.Prelude.Text a

derive instance Data.Generic.Rep.Generic (ProdFoo a) _
instance (Data.Show.Show a) => Data.Show.Show (ProdFoo a) where
  show = Data.Show.Generic.genericShow

newtype ProdBar a = ProdBar a
derive instance Data.Newtype.Newtype (ProdBar a) _
derive instance Data.Generic.Rep.Generic (ProdBar a) _
instance (Data.Show.Show a) => Data.Show.Show (ProdBar a) where
  show = Data.Show.Generic.genericShow

newtype RecBar a = RecBar { bar :: a}
derive instance Data.Newtype.Newtype (RecBar a) _
derive instance Data.Generic.Rep.Generic (RecBar a) _
instance (Data.Show.Show a) => Data.Show.Show (RecBar a) where
  show = Data.Show.Generic.genericShow

newtype RecFoo a = RecFoo { bar :: LambdaBuffers.Prelude.Text, baz :: a}
derive instance Data.Newtype.Newtype (RecFoo a) _
derive instance Data.Generic.Rep.Generic (RecFoo a) _
instance (Data.Show.Show a) => Data.Show.Show (RecFoo a) where
  show = Data.Show.Generic.genericShow
```
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef tyd@(PC.TyDef tyN tyabs _) = do
  (kw, absDoc) <- printTyAbs tyN tyabs
  case kw of
    SynonymTyDef -> return $ group $ printTyDefKw kw <+> printTyName tyN <+> absDoc
    NewtypeTyDef -> do
      ntDrvDoc <- printNewtypeDerive tyd
      gDrvDoc <- printGenericDerive tyd
      sInstDoc <- printShowInstance tyd
      return $
        vsep
          [ group $ printTyDefKw kw <+> printTyName tyN <+> absDoc
          , ntDrvDoc
          , gDrvDoc
          , sInstDoc
          ]
    DataTyDef -> do
      gDrvDoc <- printGenericDerive tyd
      sInstDoc <- printShowInstance tyd
      return $
        vsep
          [ group $ printTyDefKw kw <+> printTyName tyN <+> absDoc
          , gDrvDoc
          , sInstDoc
          ]

printTyDefKw :: TyDefKw -> Doc ann
printTyDefKw DataTyDef = "data"
printTyDefKw NewtypeTyDef = "newtype"
printTyDefKw SynonymTyDef = "type"
