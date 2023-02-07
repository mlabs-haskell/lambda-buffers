module Test.Samples.Proto.TyDef (tyDef'maybe, tyDef'incoherent) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.Helpers (
  _ConstrName,
  _Constructor,
  _Sum,
  _TupleI,
  _TyAbs,
  _TyArg,
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
    ( _TyAbs [_TyArg ("a", _Type)] $
        _Sum
          [ _Constructor (_ConstrName "Nothing") (_TupleI [])
          , _Constructor (_ConstrName "Just") (_TupleI [_TyVarI "a"])
          ]
    )

-- | B a = B Maybe
tyDef'incoherent :: P.TyDef
tyDef'incoherent =
  _TyDef
    (_tyName "B")
    ( _TyAbs [_TyArg ("a", _Type)] $
        _Sum
          [ _Constructor (_ConstrName "Nothing") (_TupleI [])
          , _Constructor (_ConstrName "B") (_TupleI [_TyRefILocal "Maybe"])
          ]
    )
