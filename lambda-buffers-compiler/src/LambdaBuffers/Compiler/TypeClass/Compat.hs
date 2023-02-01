{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClass.Compat where

import Control.Lens ((^.))
import Control.Lens.Combinators (Bifunctor (bimap), over, set, view)
import Control.Monad.State (State)
import Data.Generics.Labels (Field')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  ClassRef (CRef),
  Constraint (..),
  Instance,
  Rule ((:<=)),
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  hcat,
  list,
  parens,
  punctuate,
  viaShow,
  (<+>),
 )

si :: P.SourceInfo
si = P.SourceInfo "" (P.SourcePosition 0 0) (P.SourcePosition 0 0)

{-
    TyDefs
-}

modulename :: P.ModuleName -> [T.Text]
modulename (P.ModuleName parts _) = map (view #name) parts

defToPat :: P.TyDef -> Pat
defToPat (P.TyDef tName (P.TyAbs tArgs tBody _) _) = DecP (Name $ tName ^. #name) vars $ case tBody of
  P.SumI constrs -> toSum . NE.toList . fmap goConstr $ (constrs ^. #constructors)
  P.OpaqueI _ -> Opaque
  where
    collectFreeTyVars :: [P.TyArg] -> Pat
    collectFreeTyVars = foldr (\x acc -> Name (x ^. (#argName . #name)) :* acc) Nil

    vars = collectFreeTyVars tArgs

    goConstr :: P.Constructor -> Pat
    goConstr (P.Constructor n p) = Name (n ^. #name) := goProduct p

    goProduct :: P.Product -> Pat
    goProduct = \case
      P.RecordI (P.Record rMap _) -> toRec . NE.toList . fmap goField $ rMap
      P.TupleI (P.Tuple pList _) -> toProd $ fmap tyToPat pList

    goField :: P.Field -> Pat
    goField (P.Field n v) = Name (n ^. #name) := tyToPat v

tyToPat :: P.Ty -> Pat
tyToPat = \case
  P.TyVarI t -> VarP (t ^. #varName . #name)
  P.TyAppI tapp ->
    let fun = tyToPat $ tapp ^. #tyFunc
        ps = tyToPat <$> tapp ^. #tyArgs
     in appToPat fun ps
  P.TyRefI ref -> case ref of
    P.LocalI (P.LocalRef tn _) -> RefP Nil . Name $ tn ^. #name
    P.ForeignI (P.ForeignRef tn mn _) ->
      let mnm = modulename mn
       in RefP (ModuleName mnm) . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
  Nothing -> AppP fun p
  Just rest -> AppP fun p `appToPat` rest

prettyClassRef :: ClassRef -> Doc ()
prettyClassRef (CRef nm mn) = prettyModuleName mn <> "." <> pretty (nm ^. #name)

prettyModuleName :: P.ModuleName -> Doc ()
prettyModuleName (P.ModuleName parts _) = hcat . punctuate "." $ map (\x -> pretty $ x ^. #name) parts

prettyClass :: Class -> Doc ()
prettyClass (Class nm []) = prettyClassRef nm
prettyClass (Class nm sups) = prettyClassRef nm <+> "<=" <+> hcat (map prettyClass sups)

prettyConstraint :: Constraint -> Doc ()
prettyConstraint (C cls p) = "C" <+> parens (prettyClass cls) <+> viaShow p

prettyInstance :: Instance -> Doc ()
prettyInstance (c :<= cs) = prettyConstraint c <+> "<=" <+> list (prettyConstraint <$> cs)
