module LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef, printTraitBound, collectTyVars, printInstanceContext, printInstanceContext') where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
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
printInstanceDef :: R.PkgMap -> R.QTraitName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef pkgs rsQTraitName ty =
  let headDoc = R.printRsQTraitName rsQTraitName <+> "for" <+> printTyInner pkgs ty
      freeVars = collectTyVars ty
   in case freeVars of
        [] -> \implDoc -> "impl" <+> headDoc <+> braces (line <> implDoc)
        _ -> \implDoc ->
          "impl" <> printInstanceContext pkgs rsQTraitName freeVars
            <+> headDoc
            <+> braces (hardline <> space <> space <> implDoc)

printInstanceContext :: R.PkgMap -> R.QTraitName -> [PC.Ty] -> Doc ann
printInstanceContext pkgs rsQTraitName = printInstanceContext' pkgs [rsQTraitName]

defaultTraitBounds :: [R.QTraitName]
defaultTraitBounds = [RR.cloneTrait]

printInstanceContext' :: R.PkgMap -> [R.QTraitName] -> [PC.Ty] -> Doc ann
printInstanceContext' pkgs rsQTraitNames tys =
  align . group $ encloseSep langle rangle comma [printTraitBound pkgs (rsQTraitNames <> defaultTraitBounds) ty | ty <- tys]

printTraitBound :: R.PkgMap -> [R.QTraitName] -> PC.Ty -> Doc ann
printTraitBound pkgs qcns ty =
  let crefDocs = R.printRsQTraitName <$> qcns
      tyDoc = printTyInner pkgs ty
   in tyDoc <> colon <+> encloseSep mempty mempty "+" crefDocs

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = fmap (`PC.withInfoLess` (PC.TyVarI . PC.TyVar)) . toList . collectVars

collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars = collectVars' mempty

collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected
