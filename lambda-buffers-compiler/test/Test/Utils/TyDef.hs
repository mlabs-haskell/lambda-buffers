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

import LambdaBuffers.ProtoCompat.Types qualified as PC
import Test.Utils.Constructors (
  _ForeignRef',
  _LocalRef',
  _ModuleName,
  _Opaque,
  _ProductI,
  _SourceInfo,
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
tyDef'maybe :: PC.TyDef
tyDef'maybe =
  _TyDef
    (_TyName "Maybe")
    ( _TyAbs
        [("a", _Type)]
        [ ("Nothing", _ProductI [])
        , ("Just", _ProductI [_TyVarI "a"])
        ]
    )

tyDef'either'opaque :: PC.TyDef
tyDef'either'opaque =
  _TyDef
    (_TyName "Either")
    ( _TyAbs'
        [ ("a", _Type)
        , ("b", _Type)
        ]
        _Opaque
    )

tyDef'either :: PC.TyDef
tyDef'either =
  _TyDef
    (_TyName "Either")
    ( _TyAbs
        [ ("a", _Type)
        , ("b", _Type)
        ]
        [ ("Left", _ProductI [_TyVarI "a"])
        , ("Right", _ProductI [_TyVarI "b"])
        ]
    )

tyDef'Int :: PC.TyDef
tyDef'Int = _TyDef (_TyName "Int") (_TyAbs' mempty _Opaque)

tyDef'ntEither'saturated :: PC.TyDef
tyDef'ntEither'saturated =
  _TyDef
    (_TyName "WrapEither")
    ( _TyAbs
        mempty
        [
          ( "Foo"
          , _ProductI
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

tyDef'ntEither :: PC.TyDef
tyDef'ntEither =
  _TyDef
    (_TyName "WrapEither")
    ( _TyAbs
        [ ("a", _Type)
        , ("b", _Type)
        ]
        [ ("Foo", _ProductI [_TyApp (_TyApp (_TyRefILocal "Either") (_TyVarI "a")) (_TyVarI "b")])
        ]
    )

-- data F a = Rec F (F a)
tyDef'recDef :: PC.TyDef
tyDef'recDef =
  _TyDef
    (_TyName "F")
    ( _TyAbs
        [ ("a", _Type)
        ]
        [
          ( "Rec"
          , _ProductI
              [ _TyApp (_TyRefILocal "F") $
                  _TyApp (_TyRefILocal "F") (_TyVarI "a")
              ]
          )
        ]
    )

-- | B a = B Maybe
tyDef'incoherent :: PC.TyDef
tyDef'incoherent =
  _TyDef
    (_TyName "B")
    ( _TyAbs
        [("a", _Type)]
        [ ("Nothing", _ProductI [])
        , ("B", _ProductI [_TyRefILocal "Maybe"])
        ]
    )

-- | Foo = Bar b
tyDef'undefinedVar :: PC.TyDef
tyDef'undefinedVar =
  _TyDef
    (_TyName "Foo")
    (_TyAbs [] [("Bar", _ProductI [PC.TyVarI tyDef'undefinedVar'var])])

{- | The undefined var (i.e. "b") in tyDef'undefinedVar.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedVar'var :: PC.TyVar
tyDef'undefinedVar'var = _TyVar' "b" (_SourceInfo 1 2)

-- | Foo a = Foo Baz b
tyDef'undefinedLocalTyRef :: PC.TyDef
tyDef'undefinedLocalTyRef =
  _TyDef
    (_TyName "Foo")
    ( _TyAbs
        [("a", _Type)]
        [
          ( "Foo"
          , _ProductI
              [ PC.TyRefI tyDef'undefinedLocalTyRef'TyRef
              , _TyVarI "a"
              ]
          )
        ]
    )

{- | The undefined Local TyRef (i.e. "Baz") in tyDef'undefinedLocalTyRef.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedLocalTyRef'TyRef :: PC.TyRef
tyDef'undefinedLocalTyRef'TyRef = PC.LocalI $ _LocalRef' "Baz" (_SourceInfo 1 2)

-- | Foo a = Foo Baz b
tyDef'undefinedForeignTyRef :: PC.TyDef
tyDef'undefinedForeignTyRef =
  _TyDef
    (_TyName "Foo")
    ( _TyAbs
        [("a", _Type)]
        [
          ( "Foo"
          , _ProductI
              [ PC.TyRefI tyDef'undefinedForeignTyRef'TyRef
              , _TyVarI "a"
              ]
          )
        ]
    )

{- | The undefined Local TyRef (i.e. "Baz") in tyDef'undefinedLocalTyRef.
 Exported to see if the test identifies it correctly.
-}
tyDef'undefinedForeignTyRef'TyRef :: PC.TyRef
tyDef'undefinedForeignTyRef'TyRef = PC.ForeignI $ _ForeignRef' "Baz" (_ModuleName ["Foreign", "Module"]) (_SourceInfo 1 2)
