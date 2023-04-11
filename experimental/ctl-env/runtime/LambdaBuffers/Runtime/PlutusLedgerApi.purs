module LambdaBuffers.Runtime.PlutusLedgerApi
  ( AssetClass
  , casePlutusData
  , pdConstr
  , caseInt
  ) where

import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Types.BigNum (toBigInt, fromBigInt, zero)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr, List, Integer))
import Ctl.Internal.Types.TokenName (TokenName)
import Data.Array (filter, uncons)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Prelude ((==))

type AssetClass
  = CurrencySymbol /\ TokenName

-- | TODO(bladyjoker): Whaaai https://github.com/Plutonomicon/cardano-transaction-lib/blob/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb/src/Internal/Types/PlutusData.purs#L36
pdConstr :: BigInt -> Array PlutusData -> PlutusData
pdConstr bi pds = case fromBigInt bi of
  Nothing -> Constr zero pds
  Just bn -> Constr bn pds

casePlutusData ::
  forall r.
  (BigInt -> Array PlutusData -> r) ->
  (Array PlutusData -> r) ->
  (BigInt -> r) ->
  (PlutusData -> r) ->
  PlutusData ->
  r
casePlutusData ctorCase listCase intCase otherCase pd = case pd of
  Constr bn pds -> ctorCase (toBigInt bn) pds
  List xs -> listCase xs
  Integer bi -> intCase bi
  other -> otherCase other

caseInt :: forall a. Array (Tuple BigInt a) -> (BigInt -> a) -> BigInt -> a
caseInt cases otherCase i = case uncons (filter (\(Tuple i' _) -> i' == i) cases) of
  Just { head: Tuple _ res, tail: _ } -> res
  Nothing -> otherCase i
