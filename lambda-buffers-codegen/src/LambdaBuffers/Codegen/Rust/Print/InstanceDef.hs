module LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef, printTraitBound, collectTyVars, printInstanceContext, printInstanceContext', printTraitBound') where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as RsSyntax
import LambdaBuffers.Codegen.Rust.Print.TyDef (printTyInner)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, braces, colon, comma, encloseSep, group, hardline, hsep, langle, line, lparen, rangle, rparen, space, (<+>))

{- | `printInstanceDef rsQTraitName ty` return a function that given the printed implementation, creates an entire 'instance <rsQTraitName> <ty> where' clause.


```haskell
impl SomeClass for SomeSmallTy where
  someMethod = <implementation>

impl<A: SomeClass, B: SomeClass, C: SomeClass> SomeClass for SomeTy<A, B, C> {
  fn some_method = <implementation>
}
```
-}
printInstanceDef :: RsSyntax.QTraitName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef rsQTraitName ty =
  let headDoc = RsSyntax.printRsQTraitName rsQTraitName <+> "for" <+> printTyInner ty
      freeVars = collectTyVars ty
   in case freeVars of
        [] -> \implDoc -> "impl" <+> headDoc <+> braces (line <> implDoc)
        _ -> \implDoc ->
          "impl" <> printInstanceContext rsQTraitName freeVars
            <+> headDoc
            <+> braces (hardline <> space <> space <> implDoc)

printInstanceContext :: RsSyntax.QTraitName -> [PC.Ty] -> Doc ann
printInstanceContext rsQTraitName = printInstanceContext' [rsQTraitName]

printInstanceContext' :: [RsSyntax.QTraitName] -> [PC.Ty] -> Doc ann
printInstanceContext' rsQTraitNames tys = align . group $ encloseSep langle rangle comma ([printTraitBound rsQTraitName ty | ty <- tys, rsQTraitName <- rsQTraitNames])

printTraitBound :: RsSyntax.QTraitName -> PC.Ty -> Doc ann
printTraitBound qcn ty = printTraitBound' qcn [ty]

printTraitBound' :: RsSyntax.QTraitName -> [PC.Ty] -> Doc ann
printTraitBound' qcn tys =
  let crefDoc = RsSyntax.printRsQTraitName qcn
      tyDocs = printTyInner <$> tys
   in crefDoc <> colon <+> hsep tyDocs

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = fmap (`PC.withInfoLess` (PC.TyVarI . PC.TyVar)) . toList . collectVars

collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars = collectVars' mempty

collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected
