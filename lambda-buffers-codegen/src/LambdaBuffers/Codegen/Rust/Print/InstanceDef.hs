module LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef, printConstraint, collectTyVars, printInstanceContext, printInstanceContext', printConstraint') where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as RsSyntax
import LambdaBuffers.Codegen.Rust.Print.TyDef (printTyInner)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, braces, comma, encloseSep, group, hardline, hsep, line, lparen, rparen, space, (<+>))

{- | `printInstanceDef hsQClassName ty` return a function that given the printed implementation, creates an entire 'instance <hsQClassName> <ty> where' clause.


```haskell
instance SomeClass SomeSmallTy where
  someMethod = <implementation>

instance (SomeClass a, SomeClass b, SomeClass c) => SomeClass (SomeTy a b c) where
  someMethod = <implementation>
```
-}
printInstanceDef :: RsSyntax.QClassName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef hsQClassName ty =
  let headDoc = printConstraint hsQClassName ty
      freeVars = collectTyVars ty
   in case freeVars of
        [] -> \implDoc -> "impl" <+> headDoc <+> braces (line <> implDoc)
        _ -> \implDoc -> "impl" <+> printInstanceContext hsQClassName freeVars <+> "=>" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc

printInstanceContext :: RsSyntax.QClassName -> [PC.Ty] -> Doc ann
printInstanceContext hsQClassName = printInstanceContext' [hsQClassName]

printInstanceContext' :: [RsSyntax.QClassName] -> [PC.Ty] -> Doc ann
printInstanceContext' hsQClassNames tys = align . group $ encloseSep lparen rparen comma ([printConstraint hsQClassName ty | ty <- tys, hsQClassName <- hsQClassNames])

printConstraint :: RsSyntax.QClassName -> PC.Ty -> Doc ann
printConstraint qcn ty = printConstraint' qcn [ty]

printConstraint' :: RsSyntax.QClassName -> [PC.Ty] -> Doc ann
printConstraint' qcn tys =
  let crefDoc = RsSyntax.printRsQClassName qcn
      tyDocs = printTyInner <$> tys
   in crefDoc <+> "for" <+> hsep tyDocs

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = fmap (`PC.withInfoLess` (PC.TyVarI . PC.TyVar)) . toList . collectVars

collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars = collectVars' mempty

collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected
