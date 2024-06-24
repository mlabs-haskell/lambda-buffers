module LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef, printTraitBound, collectTyVars, printInstanceContext, printInstanceContext') where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Rust.Backend (MonadRustBackend)
import LambdaBuffers.Codegen.Rust.Print.Refs qualified as RR
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.Codegen.Rust.Print.TyDef (printTyInner)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, braces, colon, comma, encloseSep, group, hardline, langle, line, rangle, space, (<+>))

{- | `printInstanceDef rsQTraitName ty` return a function that given the printed implementation, creates an entire 'instance <rsQTraitName> <ty> where' clause.


```rust
impl SomeClass for SomeSmallTy where
  someMethod = <implementation>

impl<A: SomeClass, B: SomeClass, C: SomeClass> SomeClass for SomeTy<A, B, C> {
  fn some_method = <implementation>
}
```
-}
printInstanceDef :: MonadRustBackend m => R.QTraitName -> PC.Ty -> m (Doc ann -> Doc ann)
printInstanceDef rsQTraitName ty = do
  tyDoc <- printTyInner ty
  let headDoc = R.printRsQTraitName rsQTraitName <+> "for" <+> tyDoc
      freeVars = collectTyVars ty
  instanceCtxDoc <- printInstanceContext rsQTraitName freeVars
  return $ case freeVars of
    [] -> \implDoc -> "impl" <+> headDoc <+> braces (line <> implDoc)
    _other -> \implDoc ->
      "impl"
        <> instanceCtxDoc
        <+> headDoc
        <+> braces (hardline <> space <> space <> implDoc)

printInstanceContext :: MonadRustBackend m => R.QTraitName -> [PC.Ty] -> m (Doc ann)
printInstanceContext rsQTraitName = printInstanceContext' [rsQTraitName]

defaultTraitBounds :: [R.QTraitName]
defaultTraitBounds = [RR.cloneTrait]

printInstanceContext' :: MonadRustBackend m => [R.QTraitName] -> [PC.Ty] -> m (Doc ann)
printInstanceContext' rsQTraitNames tys = do
  traitBoundDocs <- traverse (printTraitBound (rsQTraitNames <> defaultTraitBounds)) tys
  return $ align . group $ encloseSep langle rangle comma traitBoundDocs

printTraitBound :: MonadRustBackend m => [R.QTraitName] -> PC.Ty -> m (Doc ann)
printTraitBound qcns ty = do
  let crefDocs = R.printRsQTraitName <$> qcns
  tyDoc <- printTyInner ty
  return $ tyDoc <> colon <+> encloseSep mempty mempty "+" crefDocs

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = fmap (`PC.withInfoLess` (PC.TyVarI . PC.TyVar)) . toList . collectVars

collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars = collectVars' mempty

collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected
