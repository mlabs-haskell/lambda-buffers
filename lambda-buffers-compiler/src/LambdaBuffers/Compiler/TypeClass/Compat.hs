{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClass.Compat (
  modulename,
  defToExp,
  tyToExp,
  appToExp,
  defToPat,
  tyToPat,
  appToPat,
) where

import Control.Lens ((^.))
import Control.Lens.Combinators (view)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat (
  Exp (AppE, DecE, LitE, NilE, RefE),
  ExpressionLike (nil, (*:), (*=)),
  Literal (ModuleName, Name, Opaque, TyVar),
  Pat (AppP, DecP, LitP, NilP, RefP, VarP),
  toProdE,
  toProdP,
  toRecE,
  toRecP,
  toSumE,
  toSumP,
 )

{-
    TyDefs
-}

{- |
Extract the inner [Text] from the parts inside a ProtoCompat.ModuleName
-}
modulename :: P.ModuleName -> [T.Text]
modulename (P.ModuleName parts _) = map (view #name) parts

{- |
Transform a ProtoCompat.TyDef into a Pat pattern. Note that the resulting
pattern is guaranteed to be a literal pattern (or ground term, if you like),
making the resulting Pat suitable for substitution into Rules.

(TyVarP is a literal pattern)
-}
defToExp :: P.TyDef -> Exp
defToExp (P.TyDef tName (P.TyAbs tArgs tBody _) _) = DecE (LitE . Name $ tName ^. #name) vars $ case tBody of
  P.SumI constrs -> toSumE . NE.toList . fmap goConstr $ (constrs ^. #constructors)
  P.OpaqueI _ -> LitE Opaque
  where
    collectFreeTyVars :: [P.TyArg] -> Exp
    collectFreeTyVars = foldr (\x acc -> LitE (TyVar (x ^. (#argName . #name))) *: acc) nil

    vars = collectFreeTyVars tArgs

    goConstr :: P.Constructor -> Exp
    goConstr (P.Constructor n p) = LitE (Name (n ^. #name)) *= goProduct p

    goProduct :: P.Product -> Exp
    goProduct = \case
      P.RecordI (P.Record rMap _) -> toRecE . NE.toList . fmap goField $ rMap
      P.TupleI (P.Tuple pList _) -> toProdE $ fmap tyToExp pList

    goField :: P.Field -> Exp
    goField (P.Field n v) = LitE (Name (n ^. #name)) *= tyToExp v

tyToExp :: P.Ty -> Exp
tyToExp = \case
  P.TyVarI t -> LitE . TyVar $ t ^. #varName . #name
  P.TyAppI tapp ->
    let fun = tyToExp $ tapp ^. #tyFunc
        ps = tyToExp <$> tapp ^. #tyArgs
     in appToExp fun ps
  P.TyRefI ref -> case ref of
    P.LocalI (P.LocalRef tn _) -> RefE NilE . LitE . Name $ tn ^. #name
    P.ForeignI (P.ForeignRef tn mn _) ->
      let mnm = modulename mn
       in RefE (LitE $ ModuleName mnm) . LitE . Name $ (tn ^. #name)

appToExp :: Exp -> NonEmpty Exp -> Exp
appToExp fun (p :| ps) = case NE.nonEmpty ps of
  Nothing -> AppE fun p
  Just rest -> AppE fun p `appToExp` rest

defToPat :: P.TyDef -> Pat
defToPat (P.TyDef tName (P.TyAbs tArgs tBody _) _) = DecP (LitP . Name $ tName ^. #name) vars $ case tBody of
  P.SumI constrs -> toSumP . NE.toList . fmap goConstr $ (constrs ^. #constructors)
  P.OpaqueI _ -> LitP Opaque
  where
    collectFreeTyVars :: [P.TyArg] -> Pat
    collectFreeTyVars = foldr (\x acc -> VarP (x ^. (#argName . #name)) *: acc) nil

    vars = collectFreeTyVars tArgs

    goConstr :: P.Constructor -> Pat
    goConstr (P.Constructor n p) = LitP (Name (n ^. #name)) *= goProduct p

    goProduct :: P.Product -> Pat
    goProduct = \case
      P.RecordI (P.Record rMap _) -> toRecP . NE.toList . fmap goField $ rMap
      P.TupleI (P.Tuple pList _) -> toProdP $ fmap tyToPat pList

    goField :: P.Field -> Pat
    goField (P.Field n v) = LitP (Name (n ^. #name)) *= tyToPat v

tyToPat :: P.Ty -> Pat
tyToPat = \case
  P.TyVarI t -> VarP (t ^. #varName . #name)
  P.TyAppI tapp ->
    let fun = tyToPat $ tapp ^. #tyFunc
        ps = tyToPat <$> tapp ^. #tyArgs
     in appToPat fun ps
  P.TyRefI ref -> case ref of
    P.LocalI (P.LocalRef tn _) -> RefP NilP . LitP . Name $ tn ^. #name
    P.ForeignI (P.ForeignRef tn mn _) ->
      let mnm = modulename mn
       in RefP (LitP $ ModuleName mnm) . LitP . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
  Nothing -> AppP fun p
  Just rest -> AppP fun p `appToPat` rest
