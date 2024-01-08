{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module LambdaBuffers.Runtime.Plutus.LamVal (caseIntE) where

import PlutusTx.Eq (Eq ((==)))
import PlutusTx.Integer (Integer)

{- | CaseIntE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
 HACK(bladyjoker): This is stinky as base LamVal CaseIntE should be agnostic of PlutusTx ofc. However, PlutusTx is also like its own language and should be treated like Plutarch it seems, as an eDSL with its own setup. PlutusTx doesn't handle case expressions on Integer (see https://cardano.stackexchange.com/questions/8325/how-to-do-on-chain-integer-pattern-matching) as it tries the `base.Prelude` Integer's Eq instance rather than the PlutusTx one.
-}
{-# INLINEABLE caseIntE #-}
caseIntE :: Integer -> [(Integer, a)] -> (Integer -> a) -> a
caseIntE i [] other = other i
caseIntE i ((i', val) : cases) other = if i == i' then val else caseIntE i cases other
