module LambdaBuffers.Runtime.Plutus
  ( AssetClass
  , TxInInfo(..)
  , casePlutusData
  , pdConstr
  , NotImplemented
  ) where

import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.BigNum (toBigInt, fromBigInt, zero)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr, List, Integer))
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import JS.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
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

data NotImplemented

instance eqNotImplemented :: Eq NotImplemented where
  eq _ _ = false

instance showNotImplement :: Show NotImplemented where
  show _ = "not implemented"

instance toDataNotImplemented :: ToData NotImplemented where
  toData _ = List []

instance fromDataNotImplement :: FromData NotImplemented where
  fromData _ = Nothing

-- | https://github.com/input-output-hk/plutus/blob/0f723bef8842d805f14e763fe15590cf3da622f7/plutus-ledger-api/src/PlutusLedgerApi/V2/Contexts.hs#L59
newtype TxInInfo
  = TxInInfo
  { outRef :: TransactionInput
  , resolved :: TransactionOutput
  }

derive instance newtypeTxInInfo :: Newtype TxInInfo _

derive instance genericTxInInfo :: Generic TxInInfo _

derive newtype instance eqTxInInfo :: Eq TxInInfo

instance showTxInInfo :: Show TxInInfo where
  show = genericShow

instance toDataTxInInfo :: ToData TxInInfo where
  toData (TxInInfo { outRef, resolved }) = Constr zero [ toData outRef, toData resolved ]

instance fromDataTxInInfo :: FromData TxInInfo where
  fromData (Constr n [ outRef, resolved ])
    | n == zero =
      TxInInfo
        <$> ({ outRef: _, resolved: _ } <$> fromData outRef <*> fromData resolved)
  fromData _ = Nothing
