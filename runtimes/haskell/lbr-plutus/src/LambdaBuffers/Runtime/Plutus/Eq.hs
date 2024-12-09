{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Runtime.Plutus.Eq () where

import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusTx.Eq qualified

instance PlutusTx.Eq.Eq PlutusV3.ScriptPurpose where
  (PlutusV3.Minting curSymA) == (PlutusV3.Minting curSymB) = curSymA == curSymB
  (PlutusV3.Spending txInA) == (PlutusV3.Spending txInB) = txInA == txInB
  (PlutusV3.Rewarding credA) == (PlutusV3.Rewarding credB) = credA == credB
  (PlutusV3.Certifying indexA txCertA) == (PlutusV3.Certifying indexB txCertB) = indexA == indexB && txCertA == txCertB
  (PlutusV3.Voting voterA) == (PlutusV3.Voting voterB) = voterA == voterB
  (PlutusV3.Proposing indexA pprocA) == (PlutusV3.Proposing indexB pprocB) = indexA == indexB && pprocA == pprocB
  _ == _ = False
