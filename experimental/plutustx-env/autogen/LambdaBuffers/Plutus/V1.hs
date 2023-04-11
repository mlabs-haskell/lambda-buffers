module LambdaBuffers.Plutus.V1 (Address(..)
                               , AssetClass(..)
                               , Bytes(..)
                               , Credential(..)
                               , CurrencySymbol(..)
                               , Datum(..)
                               , DatumHash(..)
                               , Extended(..)
                               , Interval(..)
                               , LowerBound(..)
                               , Map(..)
                               , POSIXTime(..)
                               , POSIXTimeRange(..)
                               , PubKeyHash(..)
                               , Redeemer(..)
                               , RedeemerHash(..)
                               , ScriptHash(..)
                               , StakingCredential(..)
                               , TokenName(..)
                               , TxId(..)
                               , TxOutRef(..)
                               , UpperBound(..)
                               , Value(..)) where

import qualified LambdaBuffers.Plutus
import qualified LambdaBuffers.Prelude
import qualified PlutusLedgerApi.V1.Address
import qualified PlutusLedgerApi.V1.Bytes
import qualified PlutusLedgerApi.V1.Credential
import qualified PlutusLedgerApi.V1.Crypto
import qualified PlutusLedgerApi.V1.Interval
import qualified PlutusLedgerApi.V1.Scripts
import qualified PlutusLedgerApi.V1.Time
import qualified PlutusLedgerApi.V1.Tx
import qualified PlutusLedgerApi.V1.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap
import qualified Prelude


type Address = PlutusLedgerApi.V1.Address.Address

type AssetClass = PlutusLedgerApi.V1.Value.AssetClass

type Bytes = PlutusLedgerApi.V1.Bytes.LedgerBytes

type Credential = PlutusLedgerApi.V1.Credential.Credential

type CurrencySymbol = PlutusLedgerApi.V1.Value.CurrencySymbol

type Datum = PlutusLedgerApi.V1.Scripts.Datum

type DatumHash = PlutusLedgerApi.V1.Scripts.DatumHash

type Extended a = PlutusLedgerApi.V1.Interval.Extended a

type Interval a = PlutusLedgerApi.V1.Interval.Interval a

type LowerBound a = PlutusLedgerApi.V1.Interval.LowerBound a

type Map k v = PlutusTx.AssocMap.Map k v

type POSIXTime = PlutusLedgerApi.V1.Time.POSIXTime

type POSIXTimeRange = PlutusLedgerApi.V1.Time.POSIXTimeRange

type PubKeyHash = PlutusLedgerApi.V1.Crypto.PubKeyHash

type Redeemer = PlutusLedgerApi.V1.Scripts.Redeemer

type RedeemerHash = PlutusLedgerApi.V1.Scripts.RedeemerHash

type ScriptHash = PlutusLedgerApi.V1.Scripts.ScriptHash

type StakingCredential = PlutusLedgerApi.V1.Credential.StakingCredential

type TokenName = PlutusLedgerApi.V1.Value.TokenName

type TxId = PlutusLedgerApi.V1.Tx.TxId

type TxOutRef = PlutusLedgerApi.V1.Tx.TxOutRef

type UpperBound a = PlutusLedgerApi.V1.Interval.UpperBound a

type Value = PlutusLedgerApi.V1.Value.Value

