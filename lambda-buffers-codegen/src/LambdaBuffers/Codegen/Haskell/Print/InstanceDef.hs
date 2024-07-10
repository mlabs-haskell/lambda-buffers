module LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDef, printConstraint, printInstanceContext, printInstanceContext', printConstraint') where

import LambdaBuffers.Codegen.Haskell.Backend (MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyInner)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, comma, encloseSep, group, hardline, hsep, lparen, rparen, space, (<+>))

{- | `printInstanceDef hsQClassName ty` return a function that given the printed implementation, creates an entire 'instance <hsQClassName> <ty> where' clause.


```haskell
instance SomeClass SomeSmallTy where
  someMethod = <implementation>

instance (SomeClass a, SomeClass b, SomeClass c) => SomeClass (SomeTy a b c) where
  someMethod = <implementation>
```
-}
printInstanceDef :: forall t m ann. MonadHaskellBackend t m => HsSyntax.QClassName -> PC.Ty -> m (Doc ann -> m (Doc ann))
printInstanceDef hsQClassName ty = do
  let freeVars = PC.collectTyVars ty
  headDoc <- printConstraint hsQClassName ty
  return $ case freeVars of
    [] -> \implDoc -> return $ "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc
    _r -> \implDoc -> do
      instanceCtxDoc <- printInstanceContext hsQClassName freeVars
      return $ "instance" <+> instanceCtxDoc <+> "=>" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc

printInstanceContext :: forall t m ann. MonadHaskellBackend t m => HsSyntax.QClassName -> [PC.Ty] -> m (Doc ann)
printInstanceContext hsQClassName = printInstanceContext' [hsQClassName]

printInstanceContext' :: forall t m ann. MonadHaskellBackend t m => [HsSyntax.QClassName] -> [PC.Ty] -> m (Doc ann)
printInstanceContext' hsQClassNames tys = do
  constraintDocs <- traverse (uncurry printConstraint) [(hsQClassName, ty) | ty <- tys, hsQClassName <- hsQClassNames]
  return $ align . group $ encloseSep lparen rparen comma constraintDocs

printConstraint :: forall t m ann. MonadHaskellBackend t m => HsSyntax.QClassName -> PC.Ty -> m (Doc ann)
printConstraint qcn ty = printConstraint' qcn [ty]

printConstraint' :: forall t m ann. MonadHaskellBackend t m => HsSyntax.QClassName -> [PC.Ty] -> m (Doc ann)
printConstraint' qcn tys = do
  let crefDoc = HsSyntax.printHsQClassName qcn
  tyDocs <- traverse printTyInner tys
  return $ crefDoc <+> hsep tyDocs
