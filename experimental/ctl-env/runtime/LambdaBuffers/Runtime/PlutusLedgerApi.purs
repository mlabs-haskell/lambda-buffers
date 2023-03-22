module LambdaBuffers.Runtime.PlutusLedgerApi (AssetClass) where

import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Types.TokenName (TokenName)
import Data.Tuple.Nested (type (/\))

type AssetClass = CurrencySymbol /\ TokenName
