module Test.Proto.TyDef (tyDef'maybe, tyDef'incoherent) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Proto.Utils (
  _TupleI,
  _TyAbs,
  _TyDef,
  _TyRefILocal,
  _TyVarI,
  _Type,
  _tyName,
 )

-- | Maybe tyDef.
tyDef'maybe :: P.TyDef
tyDef'maybe =
  _TyDef
    (_tyName "Maybe")
    ( _TyAbs
        [("a", _Type)]
        [ ("Nothing", _TupleI [])
        , ("Just", _TupleI [_TyVarI "a"])
        ]
    )

-- | B a = B Maybe
tyDef'incoherent :: P.TyDef
tyDef'incoherent =
  _TyDef
    (_tyName "B")
    ( _TyAbs
        [("a", _Type)]
        [ ("Nothing", _TupleI [])
        , ("B", _TupleI [_TyRefILocal "Maybe"])
        ]
    )
