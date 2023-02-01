module Test.Samples.Proto.SourceInfo where

import LambdaBuffers.Compiler.ProtoCompat qualified as P

-- | Empty Source Info
sourceInfo'empty :: P.SourceInfo
sourceInfo'empty = P.SourceInfo "Empty Info" (P.SourcePosition 0 0) (P.SourcePosition 0 1)
