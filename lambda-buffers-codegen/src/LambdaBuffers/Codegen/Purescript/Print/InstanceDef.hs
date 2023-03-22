module LambdaBuffers.Codegen.Purescript.Print.InstanceDef (printInstanceDef) where

import Control.Lens (view)
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursQClassName)
import LambdaBuffers.Codegen.Purescript.Print.TyDef (printTyInner)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, align, comma, encloseSep, group, hardline, lparen, rparen, space, (<+>))

printInstanceDef :: Purs.QClassName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef pursQClassName ty =
  let headDoc = printConstraint pursQClassName ty
      freeVars = collectTyVars ty
   in case freeVars of
        [] -> \implDoc -> "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc
        _ -> \implDoc -> "instance" <+> printInstanceContext pursQClassName freeVars <+> "=>" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc

printInstanceContext :: Purs.QClassName -> [PC.Ty] -> Doc ann
printInstanceContext hsQClassName tys = align . group $ encloseSep lparen rparen comma (printConstraint hsQClassName <$> tys)

printConstraint :: Purs.QClassName -> PC.Ty -> Doc ann
printConstraint qcn ty =
  let crefDoc = printPursQClassName qcn
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
