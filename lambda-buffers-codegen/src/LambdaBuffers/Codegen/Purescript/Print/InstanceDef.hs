module LambdaBuffers.Codegen.Purescript.Print.InstanceDef (printInstanceDef, printNewtypeDerive, printGenericDerive, printShowInstance) where

import Control.Lens (view, (^.))
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered qualified as OMap
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Print (importClass, importValue)
import LambdaBuffers.Codegen.Purescript.Print.Monad (MonadPrint)
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursQClassName, printPursQValName)
import LambdaBuffers.Codegen.Purescript.Print.Ty (printTyInner)
import LambdaBuffers.Codegen.Purescript.Syntax (className, normalValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, align, comma, encloseSep, equals, group, hardline, lparen, rparen, space, (<+>))

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

genericShow :: Purs.QValName
genericShow = normalValName "prelude" "Data.Show.Generic" "genericShow"

newtypeClass :: Purs.QClassName
newtypeClass = className "newtype" "Data.Newtype" "Newtype"

genericClass :: Purs.QClassName
genericClass = className "prelude" "Data.Generic.Rep" "Generic"

showClass :: Purs.QClassName
showClass = className "prelude" "Data.Show" "Show"

printNewtypeDerive :: MonadPrint m => PC.TyDef -> m (Doc ann)
printNewtypeDerive = printDerive newtypeClass

printGenericDerive :: MonadPrint m => PC.TyDef -> m (Doc ann)
printGenericDerive = printDerive genericClass

printShowInstance :: MonadPrint m => PC.TyDef -> m (Doc ann)
printShowInstance tyd = do
  importClass showClass
  importValue genericShow
  return $ printInstanceDef showClass (toSaturatedTyApp tyd) ("show" <+> equals <+> printPursQValName genericShow)

{- | `printDerive qcn tyD` prints a Purescript `derive instance` statement for a type class `qcn` for a type definition `tyd`.
 For a `Show` type class on a `Maybe a` type definition it prints
 `derive instance Show (Maybe a) _`
-}
printDerive :: MonadPrint m => Purs.QClassName -> PC.TyDef -> m (Doc ann)
printDerive qcn tyd = do
  importClass qcn
  return $ "derive instance" <+> printPursQClassName qcn <+> (printTyInner . toSaturatedTyApp $ tyd) <+> "_"

toSaturatedTyApp :: PC.TyDef -> PC.Ty
toSaturatedTyApp (PC.TyDef tyN tyAbs _) | OMap.null (tyAbs ^. #tyArgs) = PC.TyRefI $ PC.LocalI $ PC.LocalRef tyN def
toSaturatedTyApp (PC.TyDef tyN tyAbs _) =
  PC.TyAppI $
    PC.TyApp
      (PC.TyRefI $ PC.LocalI $ PC.LocalRef tyN def)
      [PC.TyVarI $ PC.TyVar (arg ^. #argName) | arg <- toList $ tyAbs ^. #tyArgs]
      def
