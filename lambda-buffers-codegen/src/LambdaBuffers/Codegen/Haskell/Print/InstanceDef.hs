module LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDef) where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyInner)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, comma, encloseSep, group, hardline, lparen, rparen, space, (<+>))

printInstanceDef :: HsSyntax.QClassName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef hsQClassName ty =
  let headDoc = printConstraint hsQClassName ty
      freeVars = collectTyVars ty
   in case freeVars of
        [] -> \implDoc -> "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc
        _ -> \implDoc -> "instance" <+> printInstanceContext hsQClassName freeVars <+> "=>" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc

printInstanceContext :: HsSyntax.QClassName -> [PC.Ty] -> Doc ann
printInstanceContext hsQClassName tys = align . group $ encloseSep lparen rparen comma (printConstraint hsQClassName <$> tys)

printConstraint :: HsSyntax.QClassName -> PC.Ty -> Doc ann
printConstraint qcn ty =
  let crefDoc = HsSyntax.printHsQClassName qcn
      tyDoc = printTyInner ty
   in crefDoc <+> tyDoc

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = fmap (`PC.withInfoLess` (PC.TyVarI . PC.TyVar)) . toList . collectVars

collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars = collectVars' mempty

collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected
