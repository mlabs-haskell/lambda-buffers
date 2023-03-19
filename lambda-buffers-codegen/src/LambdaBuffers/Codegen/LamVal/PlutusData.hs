module LambdaBuffers.Codegen.LamVal.PlutusData (deriveToPlutusDataImpl) where

import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.LamVal (Product, Record, Sum, ValueE (ErrorE, IntE, ListE), caseE, fieldE, lamE, letE, refE, (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

toPlutusDataSum :: PC.QTyName -> Sum -> ValueE
toPlutusDataSum qtyN sumTy =
  lamE
    ( \x ->
        caseE
          (qtyN, sumTy)
          x
          ( \((ctorN, ctorProdTy), xs) ->
              if isUnitProd ctorProdTy
                then integerToPlutusDataE @ findCtorName sumTy ctorN
                else
                  constrToPlutusDataE
                    @ findCtorName sumTy ctorN
                    @ ListE [toPlutusDataE fieldTy @ fieldVal | (fieldVal, fieldTy) <- zip xs ctorProdTy]
          )
    )

toPlutusDataProduct :: PC.QTyName -> Product -> ValueE
toPlutusDataProduct qtyN prodTy =
  lamE
    ( \prodVal ->
        letE
          (qtyN, prodTy, prodVal)
          ( \fieldVals ->
              case (fieldVals, prodTy) of
                ([], []) -> ErrorE "Got an empty Product type to print in `toPlutusDataProduct`"
                ([fieldVal], [fieldTy]) -> toPlutusDataE fieldTy @ fieldVal
                (fieldVals', fieldTys)
                  | length fieldVals' == length fieldTys ->
                      listToPlutusDataE
                        @ ListE
                          [ toPlutusDataE fieldTy @ fieldVal
                          | (fieldVal, fieldTy) <- zip fieldVals' fieldTys
                          ]
                _ -> ErrorE "Got mismatching number of variables"
          )
    )

toPlutusDataRecord :: PC.QTyName -> Record -> ValueE
toPlutusDataRecord qtyN recTy =
  lamE
    ( \recVal ->
        case OMap.assocs recTy of
          [] -> ErrorE "Got an empty Record type to print in `toPlutusDataRecord`"
          [(fieldName, fieldTy)] -> toPlutusDataE fieldTy @ fieldE (qtyN, fieldName) recVal
          _ ->
            listToPlutusDataE
              @ ListE
                [ toPlutusDataE fieldTy @ fieldE (qtyN, fieldName) recVal
                | (fieldName, fieldTy) <- OMap.assocs recTy
                ]
    )

deriveToPlutusDataImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either String ValueE
deriveToPlutusDataImpl mn tydefs = deriveImpl mn tydefs toPlutusDataSum toPlutusDataProduct toPlutusDataRecord

isUnitProd :: Product -> Bool
isUnitProd = null

findCtorName :: Sum -> PC.InfoLess PC.ConstrName -> ValueE
findCtorName sumTy ctorN = case OMap.findIndex ctorN sumTy of
  Nothing -> ErrorE $ "Failed find an index for constructor named\n" <> PC.withInfoLess ctorN show <> "\nin a sum type\n" <> show sumTy
  Just ix -> IntE ix

-- | `toPlutusData :: a -> PlutusData`
toPlutusDataE :: E.Ty -> ValueE
toPlutusDataE ty = refE (Just ty, "toPlutusData")

-- | `integerData :: IntE -> PlutusData`
integerToPlutusDataE :: ValueE
integerToPlutusDataE = refE (Nothing, "integerData")

-- | `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrToPlutusDataE :: ValueE
constrToPlutusDataE = refE (Nothing, "constrData")

-- | `listData :: ListE PlutusData -> PlutusData`
listToPlutusDataE :: ValueE
listToPlutusDataE = refE (Nothing, "listData")
