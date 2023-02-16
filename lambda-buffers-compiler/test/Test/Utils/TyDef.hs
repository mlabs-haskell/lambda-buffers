module Test.Utils.TyDef (
  tyDef'maybe,
  tyDef'incoherent,
  tyDef'undefinedVar,
  tyDef'undefinedVar'var,
  tyDef'undefinedlocalTyRef,
  tyDef'undefinedlocalTyRef'localTyRef,
) where

import LambdaBuffers.Compiler.ProtoCompat.Types (Ty (TyVarI))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P
import Test.Utils.Constructors (
  _LocalRef',
  _SourceInfo,
  _TupleI,
  _TyAbs,
  _TyDef,
  _TyName,
  _TyRefILocal,
  _TyVar',
  _TyVarI,
  _Type,
 )

-- | Maybe tyDef.
tyDef'maybe :: P.TyDef
tyDef'maybe =
  _TyDef
    (_TyName "Maybe")
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
    (_TyName "B")
    ( _TyAbs
        [("a", _Type)]
        [ ("Nothing", _TupleI [])
        , ("B", _TupleI [_TyRefILocal "Maybe"])
        ]
    )

-- | Foo = Bar b
tyDef'undefinedVar :: P.TyDef
tyDef'undefinedVar =
  _TyDef
    (_TyName "Foo")
    (_TyAbs [] [("Bar", _TupleI [TyVarI tyDef'undefinedVar'var])])

{- | The undefined var (i.e. "b") in tyDef'undefinedVar.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedVar'var :: P.TyVar
tyDef'undefinedVar'var = _TyVar' "b" (_SourceInfo 1 2)

-- | Foo a = Foo Baz b
tyDef'undefinedlocalTyRef :: P.TyDef
tyDef'undefinedlocalTyRef =
  _TyDef
    (_TyName "Foo")
    ( _TyAbs
        [("a", _Type)]
        [
          ( "Foo"
          , _TupleI
              [ P.TyRefI tyDef'undefinedlocalTyRef'localTyRef
              , _TyVarI "a"
              ]
          )
        ]
    )

{- | The undefined Local TyRef (i.e. "Baz") in tyDef'undefinedlocalTyRef.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedlocalTyRef'localTyRef :: P.TyRef
tyDef'undefinedlocalTyRef'localTyRef = P.LocalI $ _LocalRef' "Baz" (_SourceInfo 1 2)
