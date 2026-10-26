module LambdaBuffers.Runtime.PlutusTx.List (List, Array) where

type List a = [a]

newtype Array a = Array a
  deriving newtype (Show, Eq)
