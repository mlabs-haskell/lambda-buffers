module LambdaBuffers.Codegen.LamVal.PlutusData (deriveToPlutusDataImpl, deriveFromPlutusDataImpl) where

import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.LamVal (Product, QProduct, QRecord, QSum, Sum, ValueE (CaseE, CaseIntE, CaseListE, CtorE, ErrorE, FieldE, IntE, LamE, LetE, ListE, ProductE, RecordE, RefE), (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.ProtoCompat.Eval (fromTy)
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

toPlutusDataSum :: QSum -> ValueE
toPlutusDataSum qsum@(_, sumTy) =
  LamE
    ( \sumVal ->
        CaseE
          qsum
          sumVal
          ( \((ctorN, ctorTy), ctorVal) ->
              let ctorIx = findCtorIndex sumTy ctorN
               in if isUnitProd ctorTy
                    then integerToPlutusDataRef @ ctorIx
                    else
                      constrToPlutusDataRef
                        @ ctorIx
                        @ ListE [toPlutusDataRef fieldTy @ fieldVal | (fieldVal, fieldTy) <- zip ctorVal ctorTy]
          )
    )

fromPlutusDataSum :: E.Ty -> QSum -> ValueE
fromPlutusDataSum ty (qtyN, sumTy) =
  LamE $
    \pdVal ->
      casePlutusData
        ty
        ( \ctorIx pds ->
            CaseIntE
              ctorIx
              [ ( IntE ix
                , fromPlutusDataCtor ctorN ctorTy pds
                )
              | (ix, (ctorN, E.TyProduct ctorTy _)) <- zip [0 ..] (OMap.assocs sumTy)
              , not (isUnitProd ctorTy)
              ]
              (\_ -> failParseRef ty)
        )
        (\_ -> failParseRef ty)
        ( \i ->
            CaseIntE
              i
              [ ( IntE ix
                , succeedParseRef ty @ CtorE (qtyN, (ctorN, ctorTy)) []
                )
              | (ix, (ctorN, E.TyProduct ctorTy _)) <- zip [0 ..] (OMap.assocs sumTy)
              , isUnitProd ctorTy
              ]
              (\_ -> failParseRef ty)
        )
        (\_ -> failParseRef ty)
        pdVal
  where
    fromPlutusDataCtor ctorN ctorTy pds =
      CaseListE
        pds
        [
          ( length ctorTy
          , \pds' -> fromPlutusDataTys (zip ctorTy pds') (\xs -> succeedParseRef ty @ CtorE (qtyN, (ctorN, ctorTy)) xs)
          )
        ]
        (\_ -> failParseRef ty)

fromPlutusDataTys :: [(E.Ty, ValueE)] -> ([ValueE] -> ValueE) -> ValueE
fromPlutusDataTys pdsToParse = go pdsToParse []
  where
    go [] tot cont = cont (reverse tot)
    go ((tyX, pdX) : rest) tot cont =
      bindParseRef tyX
        @ (fromPlutusDataRef tyX @ pdX)
        @ LamE (\x -> go rest (x : tot) cont)

toPlutusDataProduct :: QProduct -> ValueE
toPlutusDataProduct qprod@(_, prodTy) =
  LamE
    ( \prodVal ->
        LetE
          qprod
          prodVal
          ( \fieldVals ->
              case (fieldVals, prodTy) of
                ([], []) -> ErrorE "Got an empty Product type to print in `toPlutusDataProduct`"
                ([fieldVal], [fieldTy]) -> toPlutusDataRef fieldTy @ fieldVal
                (fieldVals', fieldTys)
                  | length fieldVals' == length fieldTys ->
                      listToPlutusDataRef
                        @ ListE
                          [ toPlutusDataRef fieldTy @ fieldVal
                          | (fieldVal, fieldTy) <- zip fieldVals' fieldTys
                          ]
                _ -> ErrorE "Got mismatching number of variables in `toPlutusDataProduct`"
          )
    )

fromPlutusDataProduct :: E.Ty -> QProduct -> ValueE
fromPlutusDataProduct ty qprod@(_, [fieldTy]) = LamE $ \pdVal ->
  bindParseRef fieldTy
    @ (fromPlutusDataRef fieldTy @ pdVal)
    @ LamE (\fieldVal -> succeedParseRef ty @ ProductE qprod [fieldVal])
fromPlutusDataProduct ty qprod@(_, fields) =
  LamE $
    \pdVal ->
      casePlutusData
        ty
        (\_ _ -> failParseRef ty)
        ( \pds ->
            CaseListE
              pds
              [
                ( length fields
                , \pds' -> fromPlutusDataTys (zip fields pds') (\xs -> succeedParseRef ty @ ProductE qprod xs)
                )
              ]
              (\_ -> failParseRef ty)
        )
        (\_ -> failParseRef ty)
        (\_ -> failParseRef ty)
        pdVal

toPlutusDataRecord :: QRecord -> ValueE
toPlutusDataRecord (qtyN, recTy) =
  LamE
    ( \recVal ->
        case OMap.assocs recTy of
          [] -> ErrorE "Got an empty Record type to print in `toPlutusDataRecord`"
          [(fieldName, fieldTy)] -> toPlutusDataRef fieldTy @ FieldE (qtyN, fieldName) recVal
          _ ->
            listToPlutusDataRef
              @ ListE
                [ toPlutusDataRef fieldTy @ FieldE (qtyN, fieldName) recVal
                | (fieldName, fieldTy) <- OMap.assocs recTy
                ]
    )

fromPlutusDataRecord :: E.Ty -> QRecord -> ValueE
fromPlutusDataRecord ty qrec@(_, fields') = case OMap.assocs fields' of
  [field@(_, fieldTy)] -> LamE $ \pdVal ->
    bindParseRef fieldTy
      @ (fromPlutusDataRef fieldTy @ pdVal)
      @ LamE (\fieldVal -> succeedParseRef ty @ RecordE qrec [(field, fieldVal)])
  fields -> LamE $
    \pdVal ->
      casePlutusData
        ty
        (\_ _ -> failParseRef ty)
        ( \pds ->
            CaseListE
              pds
              [
                ( length fields
                , \pds' -> fromPlutusDataTys (zip (snd <$> fields) pds') (\xs -> succeedParseRef ty @ RecordE qrec (zip fields xs))
                )
              ]
              (\_ -> failParseRef ty)
        )
        (\_ -> failParseRef ty)
        (\_ -> failParseRef ty)
        pdVal

-- | Hooks
deriveToPlutusDataImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either String ValueE
deriveToPlutusDataImpl mn tydefs = deriveImpl mn tydefs toPlutusDataSum toPlutusDataProduct toPlutusDataRecord

deriveFromPlutusDataImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either String ValueE
deriveFromPlutusDataImpl mn tydefs ty = deriveImpl mn tydefs (fromPlutusDataSum (fromTy ty)) (fromPlutusDataProduct (fromTy ty)) (fromPlutusDataRecord $ fromTy ty) ty

-- | Helpers
isUnitProd :: Product -> Bool
isUnitProd = null

findCtorIndex :: Sum -> PC.InfoLess PC.ConstrName -> ValueE
findCtorIndex sumTy ctorN = case OMap.findIndex ctorN sumTy of
  Nothing -> ErrorE $ "Failed finding an index for constructor named\n" <> PC.withInfoLess ctorN show <> "\nin a sum type\n" <> show sumTy
  Just ix -> IntE ix

-- | Domain value references (functions)

-- | `toPlutusData :: a -> PlutusData`
toPlutusDataRef :: E.Ty -> ValueE
toPlutusDataRef ty = RefE (Just ty, "toPlutusData")

-- | `fromPlutusData :: PlutusData -> Parser a`
fromPlutusDataRef :: E.Ty -> ValueE
fromPlutusDataRef ty = RefE (Just ty, "fromPlutusData")

-- | `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
bindParseRef :: E.Ty -> ValueE
bindParseRef ty = RefE (Just ty, "bindParse")

-- | `failParse :: Parser a`
failParseRef :: E.Ty -> ValueE
failParseRef ty = RefE (Just ty, "failParse")

-- | `succeedParse :: a -> Parser a`
succeedParseRef :: E.Ty -> ValueE
succeedParseRef ty = RefE (Just ty, "succeedParse")

-- | `integerData :: IntE -> PlutusData`
integerToPlutusDataRef :: ValueE
integerToPlutusDataRef = RefE (Nothing, "integerData")

-- | `constrData :: IntE -> ListE PlutusData -> PlutusData`
constrToPlutusDataRef :: ValueE
constrToPlutusDataRef = RefE (Nothing, "constrData")

-- | `listData :: ListE PlutusData -> PlutusData`
listToPlutusDataRef :: ValueE
listToPlutusDataRef = RefE (Nothing, "listData")

-- | `casePlutusData :: (Int -> [PlutusData] -> a) -> ([PlutusData] -> a) -> (Int -> a) -> (PlutusData -> a) -> PlutusData -> a`
casePlutusDataRef :: E.Ty -> ValueE
casePlutusDataRef ty = RefE (Just ty, "casePlutusData")

casePlutusData :: E.Ty -> (ValueE -> ValueE -> ValueE) -> (ValueE -> ValueE) -> (ValueE -> ValueE) -> (ValueE -> ValueE) -> ValueE -> ValueE
casePlutusData ty ctorCase listCase intCase otherCase pdVal =
  casePlutusDataRef ty
    @ LamE (\ix -> LamE $ \pds -> ctorCase ix pds)
    @ LamE listCase
    @ LamE intCase
    @ LamE otherCase
    @ pdVal
