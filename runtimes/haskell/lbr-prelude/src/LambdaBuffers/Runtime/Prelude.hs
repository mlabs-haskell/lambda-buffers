{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LambdaBuffers.Runtime.Prelude (module Json, List) where

import LambdaBuffers.Runtime.Prelude.Json as Json

type List a = [a]
