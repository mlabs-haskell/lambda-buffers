let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

let DataT
    : Type
    = forall (DataT : Type) ->
      forall (I : DataT) ->
      forall (B : DataT) ->
      forall (Constr : List { ix : Integer, args : List DataT } -> DataT) ->
      forall (Map : { key : DataT, value : DataT } -> DataT) ->
      forall (L : DataT -> DataT) ->
        DataT

let IntT
    : DataT
    = \(DataT : Type) ->
      \(I : DataT) ->
      \(B : DataT) ->
      \(Constr : List { ix : Integer, args : List DataT } -> DataT) ->
      \(Map : { key : DataT, value : DataT } -> DataT) ->
      \(L : DataT -> DataT) ->
        I

let ByteStringT
    : DataT
    = \(DataT : Type) ->
      \(I : DataT) ->
      \(B : DataT) ->
      \(Constr : List { ix : Integer, args : List DataT } -> DataT) ->
      \(Map : { key : DataT, value : DataT } -> DataT) ->
      \(L : DataT -> DataT) ->
        B

let ListT
    : DataT -> DataT
    = \(x : DataT) ->
      \(DataT : Type) ->
      \(I : DataT) ->
      \(B : DataT) ->
      \(Constr : List { ix : Integer, args : List DataT } -> DataT) ->
      \(Map : { key : DataT, value : DataT } -> DataT) ->
      \(L : DataT -> DataT) ->
        L (x DataT I B Constr Map L)

let ConstrT
    : List { ix : Integer, args : List DataT } -> DataT
    = \(x : List { ix : Integer, args : List DataT }) ->
      \(_DataT : Type) ->
      \(I : _DataT) ->
      \(B : _DataT) ->
      \(Constr : List { ix : Integer, args : List _DataT } -> _DataT) ->
      \(Map : { key : _DataT, value : _DataT } -> _DataT) ->
      \(L : _DataT -> _DataT) ->
        let adapt1
            : DataT -> _DataT
            = \(y : DataT) -> y _DataT I B Constr Map L

        let adapt2
            : { ix : Integer, args : List DataT } ->
                { ix : Integer, args : List _DataT }
            = \(ys : { ix : Integer, args : List DataT }) ->
                { ix = ys.ix
                , args = Prelude.List.map DataT _DataT adapt1 ys.args
                }

        in  Constr
              ( Prelude.List.map
                  { ix : Integer, args : List DataT }
                  { ix : Integer, args : List _DataT }
                  adapt2
                  x
              )

let MapT
    : { key : DataT, value : DataT } -> DataT
    = \(x : { key : DataT, value : DataT }) ->
      \(_DataT : Type) ->
      \(I : _DataT) ->
      \(B : _DataT) ->
      \(Constr : List { ix : Integer, args : List _DataT } -> _DataT) ->
      \(Map : { key : _DataT, value : _DataT } -> _DataT) ->
      \(L : _DataT -> _DataT) ->
        let adapt
            : DataT -> _DataT
            = \(y : DataT) -> y _DataT I B Constr Map L

        in  Map { key = adapt x.key, value = adapt x.value }

let test
    : List { ix : Integer, args : List DataT }
    = [ { ix = +1, args = [] : List DataT } ]

let Class = < JSON | PlutusData | Eq >

let Schema
    : Type
    = { name : Text, type : DataT, instances : List Class }

let Module
    : Type
    = { imports : List Text, defs : List Schema }

let inner
    : Schema -> DataT
    = \(y : Schema) -> y.type

let myModule
    : Module
    = { imports = [] : List Text
      , defs =
          let TypeA =
                { name = "TypeA"
                , type = ListT ByteStringT
                , instances = [ Class.Eq ]
                }

          let TypeB =
                { name = "TypeB"
                , type = MapT { key = IntT, value = ListT IntT }
                , instances = [ Class.JSON ]
                }

          let TypeC =
                { name = "TypeC"
                , type = ListT (inner TypeB)
                , instances = [ Class.PlutusData ]
                }

          in  [ TypeA, TypeB, TypeC ] : List Schema
      }

in  myModule
