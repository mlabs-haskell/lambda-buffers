module LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl) where

import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.LamVal (Field, QProduct, QRecord, QSum, ValueE (CaseE, FieldE, LamE, LetE, RefE), (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC

-- | Eq on values of a Sum type
eqSum :: QSum -> ValueE
eqSum qsum =
  LamE
    ( \l ->
        LamE
          ( \r ->
              CaseE
                qsum
                l
                ( \(ctorTyL, lxs) ->
                    CaseE
                      qsum
                      r
                      ( \(ctorTyR, rxs) ->
                          if fst ctorTyL == fst ctorTyR
                            then
                              foldl
                                (\tot (lx, rx, ty) -> andE @ tot @ (eqE ty @ lx @ rx))
                                trueE
                                (zip3 lxs rxs (snd ctorTyL))
                            else falseE
                      )
                )
          )
    )

-- | Eq on values of a Product type
eqProduct :: QProduct -> ValueE
eqProduct qprod@(_, prodTy) =
  LamE
    ( \l ->
        LamE
          ( \r ->
              LetE
                qprod
                l
                ( \lxs ->
                    LetE
                      qprod
                      r
                      ( \rxs ->
                          foldl
                            (\tot (lx, rx, ty) -> andE @ tot @ (eqE ty @ lx @ rx))
                            trueE
                            (zip3 lxs rxs prodTy)
                      )
                )
          )
    )

-- | Eq on values of a Record type
eqRecord :: QRecord -> ValueE
eqRecord (qtyN, recTy) =
  LamE
    ( \l ->
        LamE
          ( \r ->
              foldl
                (\tot field -> andE @ tot @ eqField qtyN field l r)
                trueE
                (OMap.assocs recTy)
          )
    )

eqField :: PC.QTyName -> Field -> ValueE -> ValueE -> ValueE
eqField qtyN (fieldName, fieldTy) l r = eqE fieldTy @ FieldE (qtyN, fieldName) l @ FieldE (qtyN, fieldName) r

-- | Hooks
deriveEqImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either String ValueE
deriveEqImpl mn tydefs = deriveImpl mn tydefs eqSum eqProduct eqRecord

-- | Domain value references

-- | `eq :: a -> a -> Bool`
eqE :: LT.Ty -> ValueE
eqE ty = RefE ([ty], "eq")

-- | `false :: Bool`
falseE :: ValueE
falseE = RefE ([], "false")

-- | `true :: Bool`
trueE :: ValueE
trueE = RefE ([], "true")

-- | `and :: Bool -> Bool -> Bool`
andE :: ValueE
andE = RefE ([], "and")
