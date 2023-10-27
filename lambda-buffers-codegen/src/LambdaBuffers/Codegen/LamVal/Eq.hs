module LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl) where

import Control.Exception qualified as Exception
import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.LamVal (Field, QProduct, QRecord, QSum, ValueE (CaseE, FieldE, LamE, LetE, RefE), (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P

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
                            then eqListHelper lxs rxs (snd ctorTyL)
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
                      ( \rxs -> eqListHelper lxs rxs prodTy
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
              let eqFieldExprs = map (\field -> eqField qtyN field l r) $ OMap.assocs recTy
               in if null eqFieldExprs
                    then trueE
                    else
                      foldl1
                        (\tot eqFieldExpr -> andE @ tot @ eqFieldExpr)
                        eqFieldExprs
          )
    )

eqField :: PC.QTyName -> Field -> ValueE -> ValueE -> ValueE
eqField qtyN (fieldName, fieldTy) l r = eqE fieldTy @ FieldE (qtyN, fieldName) l @ FieldE (qtyN, fieldName) r

{- | 'eqListHelper' is an internal function which equates two lists of 'ValueE'
 with their type pairwise.

 Preconditions:
  - All input lists are the same length
-}
eqListHelper :: [ValueE] -> [ValueE] -> [LT.Ty] -> ValueE
eqListHelper lxs rxs tys =
  Exception.assert preconditionAssertion $
    let eqedLxsRxsTys = map (\(lx, rx, ty) -> eqE ty @ lx @ rx) $ zip3 lxs rxs tys
     in if null eqedLxsRxsTys
          then trueE
          else foldl1 (\tot eqExpr -> andE @ tot @ eqExpr) eqedLxsRxsTys
  where
    preconditionAssertion =
      let lxsLength = length lxs
          rxsLength = length rxs
          tysLength = length tys
       in lxsLength == rxsLength && rxsLength == tysLength

-- | Hooks
deriveEqImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either P.InternalError ValueE
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
