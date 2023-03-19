module LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDefs) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Haskell.Print.Monad (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Names (printHsQClassName)
import LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyInner)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print (ctxConfig, ctxModule)
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, comma, encloseSep, hardline, lparen, rparen, space, (<+>))

printInstanceDefs :: MonadPrint m => PC.Derive -> m (Map H.QClassName (Doc ann -> Doc ann))
printInstanceDefs drv = do
  mn <- asks (view $ ctxModule . #moduleName)
  classes <- asks (view $ ctxConfig . C.classes)
  hsQClassNames <- case Map.lookup (PC.qualifyClassRef mn (drv ^. #constraint . #classRef)) classes of
    Nothing -> throwError (drv ^. #constraint . #sourceInfo, "TODO(bladyjoker): Internal error: Missing classes mapping")
    Just hsQClassNames -> return hsQClassNames
  return $ Map.fromList ((\hsQcn -> (hsQcn, printInstanceDef hsQcn (drv ^. #constraint . #argument))) <$> hsQClassNames)

printInstanceDef :: H.QClassName -> PC.Ty -> (Doc ann -> Doc ann)
printInstanceDef hsQClassName ty =
  let headDoc = printConstraint hsQClassName ty
      contextDocs = printConstraint hsQClassName <$> collectTyVars ty
   in case toList contextDocs of
        [] -> \implDoc -> "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc
        ctxDocs -> \implDoc -> "instance" <+> encloseSep lparen rparen comma ctxDocs <+> "=>" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc

printConstraint :: H.QClassName -> PC.Ty -> Doc ann
printConstraint qcn ty =
  let crefDoc = printHsQClassName qcn
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
