module LambdaBuffers.Runtime.PlutusLedgerApi (pdConstr, AssetClass) where

import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Types.TokenName (TokenName)
import Data.Tuple.Nested (type (/\))
import Ctl.Internal.Types.BigNum (fromBigInt, zero)
import Data.BigInt (BigInt)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr))
import Data.Maybe (Maybe(Nothing, Just))

type AssetClass = CurrencySymbol /\ TokenName

-- | TODO(bladyjoker): Whaaai https://github.com/Plutonomicon/cardano-transaction-lib/blob/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb/src/Internal/Types/PlutusData.purs#L36
pdConstr :: BigInt -> Array PlutusData -> PlutusData
pdConstr bi pds = case fromBigInt bi of
  Nothing -> Constr zero pds
  Just bn -> Constr bn pds
