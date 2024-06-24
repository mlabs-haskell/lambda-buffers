module LambdaBuffers.Runtime.PlutusTx.NotImplemented (Set) where

import GHC.TypeError qualified as GHC
import PlutusTx qualified
import PlutusTx.Eq qualified as PlutusTx

data Set a

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented for PlutusTx") => PlutusTx.FromData (Set a) where
  fromBuiltinData _ = error "unreachable"

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented for PlutusTx") => PlutusTx.ToData (Set a) where
  toBuiltinData _ = error "unreachable"

instance GHC.TypeError ('GHC.Text "LambdaBuffers Prelude.Set not implemented for PlutusTx") => PlutusTx.Eq (Set a) where
  (==) _l _r = error "unreachable"
