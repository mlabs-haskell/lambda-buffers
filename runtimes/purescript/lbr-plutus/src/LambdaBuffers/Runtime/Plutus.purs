module LambdaBuffers.Runtime.Plutus
  ( AssetClass
  , TxInInfo(..)
  , caseInt
  , casePlutusData
  , pdConstr
  )
  where

import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.BigNum (toBigInt, fromBigInt, zero)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr, List, Integer))
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (filter, uncons)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Prelude (class Eq, class Show, (<$>), (<*>), (==))

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

-- | https://github.com/input-output-hk/plutus/blob/0f723bef8842d805f14e763fe15590cf3da622f7/plutus-ledger-api/src/PlutusLedgerApi/V2/Contexts.hs#L59
newtype TxInInfo = TxInInfo
    { outRef   :: TransactionInput
    , resolved :: TransactionOutput
    }

derive instance Newtype TxInInfo _
derive instance Generic TxInInfo _
derive newtype instance Eq TxInInfo

instance Show TxInInfo where
  show = genericShow

instance ToData TxInInfo where
  toData (TxInInfo { outRef, resolved }) =
    Constr zero [ toData outRef, toData resolved ]

instance FromData TxInInfo where
  fromData (Constr n [ outRef, resolved ]) | n == zero =
    TxInInfo <$>
      ({ outRef: _, resolved: _ } <$> fromData outRef <*> fromData resolved)
  fromData _ = Nothing

