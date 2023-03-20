module LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl) where

import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Codegen.LamVal (Field, Product, Record, Sum, ValueE, caseE, fieldE, lamE, letE, refE, (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

-- | `eq` function encoding parametrized by a single `E.Ty` type `eq :: a -> a -> Bool`.
eqE :: E.Ty -> ValueE
eqE ty = refE (Just ty, "eq")

falseE :: ValueE
falseE = refE (Nothing, "false")

trueE :: ValueE
trueE = refE (Nothing, "true")

andE :: ValueE
andE = refE (Nothing, "and")

eqSum :: PC.QTyName -> Sum -> ValueE
eqSum qtyN sumTy =
  lamE
    ( \l ->
        lamE
          ( \r ->
              caseE
                (qtyN, sumTy)
                l
                ( \(ctorTyL, lxs) ->
                    caseE
                      (qtyN, sumTy)
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

{- | Eq on values of a Product type.

module Foo

product Foo a b = a b

Would translate into

data Foo a b = MkFoo a b

(==) = \l -> \r -> let MkFoo x1 x2 = l in let MkFoo x3 x4 = r in x1 == x3 && x2 == x4 && true
-}
eqProduct :: PC.QTyName -> Product -> ValueE
eqProduct qtyN prodTy =
  lamE
    ( \l ->
        lamE
          ( \r ->
              letE
                (qtyN, prodTy, l)
                ( \lxs ->
                    letE
                      (qtyN, prodTy, r)
                      ( \rxs ->
                          foldl
                            (\tot (lx, rx, ty) -> andE @ tot @ (eqE ty @ lx @ rx))
                            trueE
                            (zip3 lxs rxs prodTy)
                      )
                )
          )
    )

{- | Eq on values of a Record type.

module Foo

record Foo a b = {foo : a, bar : b}

Would translate into

data Foo a b = MkFoo {foo'foo :: a, foo'bar :: b}

(==) = \l -> \r -> foo'foo l == foo'foo r && foo'bar l == foo'bar r && true
-}
eqRecord :: PC.QTyName -> Record -> ValueE
eqRecord qtyN recTy =
  lamE
    ( \l ->
        lamE
          ( \r ->
              foldl
                (\tot field -> andE @ tot @ eqField qtyN field l r)
                trueE
                (OMap.assocs recTy)
          )
    )

eqField :: PC.QTyName -> Field -> ValueE -> ValueE -> ValueE
eqField qtyN (fieldName, fieldTy) l r = eqE fieldTy @ fieldE (qtyN, fieldName) l @ fieldE (qtyN, fieldName) r

deriveEqImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either String ValueE
deriveEqImpl mn tydefs = deriveImpl mn tydefs eqSum eqProduct eqRecord
