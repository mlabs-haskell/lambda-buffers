let List/map = https://prelude.dhall-lang.org/v16.0.0/List/map

let Lang = < Haskell | PureScript | Rust | TypeScript >

let Ty
    : Type
    = forall (Ty : Type) ->
      forall (IntT : Ty) ->
      forall (BooleanT : Ty) ->
      forall (StringT : Ty) ->
      forall (SetT : Ty -> Ty) ->
      forall (MapT : { key : Ty, value : Ty } -> Ty) ->
      forall (ListT : Ty -> Ty) ->
      forall (UnitT : Ty) ->
      forall (MaybeT : Ty -> Ty) ->
      forall  ( SumT
              : List { constr : Text, ix : Integer, args : List Ty } -> Ty
              ) ->
      forall (RecT : List { label : Text, field : Ty } -> Ty) ->
      forall (VarT : Natural -> Ty) ->
      forall  ( OpaqueT
              : { lang : Lang
                , package : Text
                , srcModule : Text
                , srcName : Text
                } ->
                  Ty
              ) ->
      forall (AppT : Ty -> Ty -> Ty) ->
        Ty

let Int
    : Ty
    = \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        IntT

let Boolean
    : Ty
    = \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        BooleanT

let String
    : Ty
    = \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        StringT

let Set
    : Ty -> Ty
    = \(x : Ty) ->
      \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        SetT
          ( x
              Ty
              IntT
              BooleanT
              StringT
              SetT
              MapT
              ListT
              UnitT
              MaybeT
              SumT
              RecT
              VarT
              OpaqueT
              AppT
          )

let Map
    : { key : Ty, value : Ty } -> Ty
    = \(x : { key : Ty, value : Ty }) ->
      \(_Ty : Type) ->
      \(IntT : _Ty) ->
      \(BooleanT : _Ty) ->
      \(StringT : _Ty) ->
      \(SetT : _Ty -> _Ty) ->
      \(MapT : { key : _Ty, value : _Ty } -> _Ty) ->
      \(ListT : _Ty -> _Ty) ->
      \(UnitT : _Ty) ->
      \(MaybeT : _Ty -> _Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List _Ty } -> _Ty) ->
      \(RecT : List { label : Text, field : _Ty } -> _Ty) ->
      \(VarT : Natural -> _Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            _Ty
        ) ->
      \(AppT : _Ty -> _Ty -> _Ty) ->
        let adapt
            : Ty -> _Ty
            = \(y : Ty) ->
                y
                  _Ty
                  IntT
                  BooleanT
                  StringT
                  SetT
                  MapT
                  ListT
                  UnitT
                  MaybeT
                  SumT
                  RecT
                  VarT
                  OpaqueT
                  AppT

        in  MapT { key = adapt x.key, value = adapt x.value }

let List_
    : Ty -> Ty
    = \(x : Ty) ->
      \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        ListT
          ( x
              Ty
              IntT
              BooleanT
              StringT
              SetT
              MapT
              ListT
              UnitT
              MaybeT
              SumT
              RecT
              VarT
              OpaqueT
              AppT
          )

let Unit
    : Ty
    = \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        UnitT

let Maybe
    : Ty -> Ty
    = \(x : Ty) ->
      \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        MaybeT
          ( x
              Ty
              IntT
              BooleanT
              StringT
              SetT
              MapT
              ListT
              UnitT
              MaybeT
              SumT
              RecT
              VarT
              OpaqueT
              AppT
          )

let Sum
    : List { constr : Text, ix : Integer, args : List Ty } -> Ty
    = \(x : List { constr : Text, ix : Integer, args : List Ty }) ->
      \(_Ty : Type) ->
      \(IntT : _Ty) ->
      \(BooleanT : _Ty) ->
      \(StringT : _Ty) ->
      \(SetT : _Ty -> _Ty) ->
      \(MapT : { key : _Ty, value : _Ty } -> _Ty) ->
      \(ListT : _Ty -> _Ty) ->
      \(UnitT : _Ty) ->
      \(MaybeT : _Ty -> _Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List _Ty } -> _Ty) ->
      \(RecT : List { label : Text, field : _Ty } -> _Ty) ->
      \(VarT : Natural -> _Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            _Ty
        ) ->
      \(AppT : _Ty -> _Ty -> _Ty) ->
        let adapt
            : Ty -> _Ty
            = \(y : Ty) ->
                y
                  _Ty
                  IntT
                  BooleanT
                  StringT
                  SetT
                  MapT
                  ListT
                  UnitT
                  MaybeT
                  SumT
                  RecT
                  VarT
                  OpaqueT
                  AppT

        let adapt2
            : { constr : Text, ix : Integer, args : List Ty } ->
                { constr : Text, ix : Integer, args : List _Ty }
            = \(z : { constr : Text, ix : Integer, args : List Ty }) ->
                { constr = z.constr
                , ix = z.ix
                , args = List/map Ty _Ty adapt z.args
                }

        in  SumT
              ( List/map
                  { constr : Text, ix : Integer, args : List Ty }
                  { constr : Text, ix : Integer, args : List _Ty }
                  adapt2
                  x
              )

let LT = { label : Text, field : Ty }

let Rec
    : List LT -> Ty
    = \(x : List LT) ->
      \(_Ty : Type) ->
      \(IntT : _Ty) ->
      \(BooleanT : _Ty) ->
      \(StringT : _Ty) ->
      \(SetT : _Ty -> _Ty) ->
      \(MapT : { key : _Ty, value : _Ty } -> _Ty) ->
      \(ListT : _Ty -> _Ty) ->
      \(UnitT : _Ty) ->
      \(MaybeT : _Ty -> _Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List _Ty } -> _Ty) ->
      \(RecT : List { label : Text, field : _Ty } -> _Ty) ->
      \(VarT : Natural -> _Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            _Ty
        ) ->
      \(AppT : _Ty -> _Ty -> _Ty) ->
        let adapt
            : Ty -> _Ty
            = \(y : Ty) ->
                y
                  _Ty
                  IntT
                  BooleanT
                  StringT
                  SetT
                  MapT
                  ListT
                  UnitT
                  MaybeT
                  SumT
                  RecT
                  VarT
                  OpaqueT
                  AppT

        let adapt2
            : LT -> { label : Text, field : _Ty }
            = \(z : LT) -> { label = z.label, field = adapt z.field }

        in  RecT (List/map LT { label : Text, field : _Ty } adapt2 x)

let Var
    : Natural -> Ty
    = \(x : Natural) ->
      \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        VarT x

let Import
    : { lang : Lang, package : Text, srcModule : Text, srcName : Text } -> Ty
    = \ ( x
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text }
        ) ->
      \(Ty : Type) ->
      \(IntT : Ty) ->
      \(BooleanT : Ty) ->
      \(StringT : Ty) ->
      \(SetT : Ty -> Ty) ->
      \(MapT : { key : Ty, value : Ty } -> Ty) ->
      \(ListT : Ty -> Ty) ->
      \(UnitT : Ty) ->
      \(MaybeT : Ty -> Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List Ty } -> Ty) ->
      \(RecT : List { label : Text, field : Ty } -> Ty) ->
      \(VarT : Natural -> Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            Ty
        ) ->
      \(AppT : Ty -> Ty -> Ty) ->
        OpaqueT x

let Class = < JSON | PlutusData | Eq >

let Ap
    : Ty -> Ty -> Ty
    = \(a : Ty) ->
      \(b : Ty) ->
      \(_Ty : Type) ->
      \(IntT : _Ty) ->
      \(BooleanT : _Ty) ->
      \(StringT : _Ty) ->
      \(SetT : _Ty -> _Ty) ->
      \(MapT : { key : _Ty, value : _Ty } -> _Ty) ->
      \(ListT : _Ty -> _Ty) ->
      \(UnitT : _Ty) ->
      \(MaybeT : _Ty -> _Ty) ->
      \(SumT : List { constr : Text, ix : Integer, args : List _Ty } -> _Ty) ->
      \(RecT : List { label : Text, field : _Ty } -> _Ty) ->
      \(VarT : Natural -> _Ty) ->
      \ ( OpaqueT
        : { lang : Lang, package : Text, srcModule : Text, srcName : Text } ->
            _Ty
        ) ->
      \(AppT : _Ty -> _Ty -> _Ty) ->
        let adapt
            : Ty -> _Ty
            = \(y : Ty) ->
                y
                  _Ty
                  IntT
                  BooleanT
                  StringT
                  SetT
                  MapT
                  ListT
                  UnitT
                  MaybeT
                  SumT
                  RecT
                  VarT
                  OpaqueT
                  AppT

        in  AppT (adapt a) (adapt b)

let InstanceDecl = { type : Ty, decls : List Class }

let Module
    : Type
    = { types : List Ty, instances : List InstanceDecl }

let ARec
    : Ty
    = Rec [ { label = "BoolField", field = Boolean } ]

let AnImport
    : Ty
    = Import
        { lang = Lang.Haskell
        , package = "lens"
        , srcModule = "Control.Lens.Internal.Bazaar"
        , srcName = "Bazaar"
        }

let MayB
    : Ty
    = Sum
        [ { constr = "Nada", ix = +0, args = [] : List Ty }
        , { constr = "Just", ix = +1, args = [ Var 1 ] }
        ]

let Either
    : Ty
    = Sum
        [ { constr = "Left", ix = +0, args = [ Var 1 ] }
        , { constr = "Right", ix = +1, args = [ Var 2 ] }
        ]

let EitherIntX
    : Ty
    = Ap Either Int

let EitherIntBool
    : Ty
    = Ap Boolean EitherIntX

in  { types = [ ARec, AnImport, MayB, Either, EitherIntX, EitherIntBool ]
    , instances =
      [ { type = MayB, instances = [ Class.JSON, Class.Eq, Class.PlutusData ] }
      ]
    }
