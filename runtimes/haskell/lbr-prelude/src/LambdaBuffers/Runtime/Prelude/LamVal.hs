module LambdaBuffers.Runtime.Prelude.LamVal (caseIntE) where

{- | CaseIntE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
 HACK(bladyjoker): This one is not used, but the LambdaBuffers.Runtime.Plutus.LamVal one. We're just lucky that PlutusData encodings only use this functionality.
-}
caseIntE :: Integer -> [(Integer, a)] -> (Integer -> a) -> a
caseIntE i [] other = other i
caseIntE i ((i', val) : cases) other = if i == i' then val else caseIntE i cases other
