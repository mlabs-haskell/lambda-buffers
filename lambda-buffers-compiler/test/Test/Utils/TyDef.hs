module Test.Utils.TyDef (
  tyDef'maybe,
  tyDef'incoherent,
  tyDef'undefinedVar,
  tyDef'undefinedVar'var,
  tyDef'undefinedLocalTyRef,
  tyDef'undefinedLocalTyRef'TyRef,
  tyDef'undefinedForeignTyRef,
  tyDef'undefinedForeignTyRef'TyRef,
  tyDef'recDef,
  tyDef'either,
  tyDef'ntEither,
  tyDef'either'opaque,
  tyDef'Int,
  tyDef'ntEither'saturated,
) where

import LambdaBuffers.Compiler.ProtoCompat.Types (Ty (TyVarI))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P
import Test.Utils.Constructors (
  _ForeignRef',
  _LocalRef',
  _ModuleName,
  _Opaque,
  _SourceInfo,
  _TupleI,
  _TyAbs,
  _TyAbs',
  _TyApp,
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

tyDef'either'opaque :: P.TyDef
tyDef'either'opaque =
  _TyDef
    (_TyName "Either")
    ( _TyAbs'
        [ ("a", _Type)
        , ("b", _Type)
        ]
        _Opaque
    )

tyDef'either :: P.TyDef
tyDef'either =
  _TyDef
    (_TyName "Either")
    ( _TyAbs
        [ ("a", _Type)
        , ("b", _Type)
        ]
        [ ("Left", _TupleI [_TyVarI "a"])
        , ("Right", _TupleI [_TyVarI "b"])
        ]
    )

tyDef'Int :: P.TyDef
tyDef'Int = _TyDef (_TyName "Int") (_TyAbs' mempty _Opaque)

tyDef'ntEither'saturated :: P.TyDef
tyDef'ntEither'saturated =
  _TyDef
    (_TyName "WrapEither")
    ( _TyAbs
        mempty
        [
          ( "Foo"
          , _TupleI
              [ _TyApp
                  ( _TyApp
                      (_TyRefILocal "Either")
                      (_TyRefILocal "Int")
                  )
                  (_TyRefILocal "Int")
              ]
          )
        ]
    )

tyDef'ntEither :: P.TyDef
tyDef'ntEither =
  _TyDef
    (_TyName "WrapEither")
    ( _TyAbs
        [ ("a", _Type)
        , ("b", _Type)
        ]
        [ ("Foo", _TupleI [_TyApp (_TyApp (_TyRefILocal "Either") (_TyVarI "a")) (_TyVarI "b")])
        ]
    )

-- data F a = Rec F (F a)
tyDef'recDef :: P.TyDef
tyDef'recDef =
  _TyDef
    (_TyName "F")
    ( _TyAbs
        [ ("a", _Type)
        ]
        [
          ( "Rec"
          , _TupleI
              [ _TyApp (_TyRefILocal "F") $
                  _TyApp (_TyRefILocal "F") (_TyVarI "a")
              ]
          )
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
tyDef'undefinedLocalTyRef :: P.TyDef
tyDef'undefinedLocalTyRef =
  _TyDef
    (_TyName "Foo")
    ( _TyAbs
        [("a", _Type)]
        [
          ( "Foo"
          , _TupleI
              [ P.TyRefI tyDef'undefinedLocalTyRef'TyRef
              , _TyVarI "a"
              ]
          )
        ]
    )

{- | The undefined Local TyRef (i.e. "Baz") in tyDef'undefinedLocalTyRef.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedLocalTyRef'TyRef :: P.TyRef
tyDef'undefinedLocalTyRef'TyRef = P.LocalI $ _LocalRef' "Baz" (_SourceInfo 1 2)

-- | Foo a = Foo Baz b
tyDef'undefinedForeignTyRef :: P.TyDef
tyDef'undefinedForeignTyRef =
  _TyDef
    (_TyName "Foo")
    ( _TyAbs
        [("a", _Type)]
        [
          ( "Foo"
          , _TupleI
              [ P.TyRefI tyDef'undefinedForeignTyRef'TyRef
              , _TyVarI "a"
              ]
          )
        ]
    )

{- | The undefined Local TyRef (i.e. "Baz") in tyDef'undefinedLocalTyRef.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedForeignTyRef'TyRef :: P.TyRef
tyDef'undefinedForeignTyRef'TyRef = P.ForeignI $ _ForeignRef' "Baz" (_ModuleName ["Foreign", "Module"]) (_SourceInfo 1 2)
