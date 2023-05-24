module LambdaBuffers.Codegen.LamVal.Derive (deriveImpl) where

import LambdaBuffers.Codegen.LamVal (ProductImpl, RecordImpl, SumImpl, ValueE)
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC

deriveImpl' ::
  PC.ModuleName ->
  PC.TyDefs ->
  SumImpl ->
  ProductImpl ->
  RecordImpl ->
  PC.QTyName ->
  PC.Ty ->
  Either String ValueE
deriveImpl' mn tydefs sumImpl productImpl recordImpl qtyN ty = case LT.runEval mn tydefs ty of
  Left err -> Left $ "PC.Ty evaluation failed while trying to derive an implementation\n" <> show err
  Right (LT.TySum s _) -> Right $ sumImpl (qtyN, s)
  Right (LT.TyProduct p _) -> Right $ productImpl (qtyN, p)
  Right (LT.TyRecord r _) -> Right $ recordImpl (qtyN, r)
  Right wrongTy -> Left $ "Type evaluation resulted in an underivable `Ty`\n" <> show wrongTy

deriveImpl ::
  PC.ModuleName ->
  PC.TyDefs ->
  SumImpl ->
  ProductImpl ->
  RecordImpl ->
  PC.Ty ->
  Either String ValueE
deriveImpl mn tydefs sumImpl productImpl recordImpl ty@(PC.TyRefI tr) = deriveImpl' mn tydefs sumImpl productImpl recordImpl (PC.qualifyTyRef mn tr) ty
deriveImpl mn tydefs sumImpl productImpl recordImpl ty@(PC.TyAppI (PC.TyApp (PC.TyRefI tr) _ _)) = deriveImpl' mn tydefs sumImpl productImpl recordImpl (PC.qualifyTyRef mn tr) ty
deriveImpl _ _ _ _ _ ty = Left $ "I can only derive implementations for `TyRef` and `TyApp`, got\n" <> show ty
