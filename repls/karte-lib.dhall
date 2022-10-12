{- Schema model Dhall example

```schema-dsl

opaque Integer
opaque Bool
opaque Text
opaque Map k v
opaque Set a
opaque List a

data Unit = Unit
data Proxy a
data Maybe a = Just a | Nothing
data Either a b  = Left a | Right b
data MyType a = MyType {
  field :: Maybe a,
  field2 :: Integer
}
```
-}
let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

let Map = Prelude.Map.Type

let Field = Text

let Constructor = Text

let TypeName = Text

let ArgName = Text

let TypeApp
    : Type
    = { bound : List Text, tyName : TypeName }

let ProductArgTerm
    : Type
    = < App : TypeApp | Variable : ArgName >

let ProductTerm
    : Type
    = < Record : Map Field ProductArgTerm
      | Product : List ProductArgTerm
      | Empty
      >

let TypeTerm
    : Type
    = < Opaque | Sum : Map Constructor ProductTerm >

let TypeAbsTerm
    : Type
    = { arguments : List Text, typeTerm : TypeTerm }

let TypeDefTerm
    : Type
    = { tyName : TypeName, body : TypeAbsTerm }

let ModuleTerm
    : Type
    = { name : Text
      , bindings : Map TypeName TypeDefTerm
      , constraints : List Text
      }

let emptyArgs
    : List Text
    = [] : List Text

let opaque
    : TypeName -> List Text -> { mapKey : TypeName, mapValue : TypeDefTerm }
    = \(tyName : TypeName) ->
      \(args : List Text) ->
        { mapKey = tyName
        , mapValue =
          { tyName, body = { arguments = args, typeTerm = TypeTerm.Opaque } }
        }

let data
    : TypeName ->
      List Text ->
      Map Constructor ProductTerm ->
        { mapKey : TypeName, mapValue : TypeDefTerm }
    = \(tyName : TypeName) ->
      \(args : List Text) ->
      \(constructors : Map Constructor ProductTerm) ->
        { mapKey = tyName
        , mapValue =
          { tyName
          , body = { arguments = args, typeTerm = TypeTerm.Sum constructors }
          }
        }

let nullary = ProductTerm.Empty

let var =
      \(arg : ArgName) -> ProductTerm.Product [ ProductArgTerm.Variable arg ]

let app =
      \(tyName : TypeName) ->
      \(args : List ArgName) ->
        ProductTerm.Product [ ProductArgTerm.App { tyName, bound = args } ]

let mymod
    : ModuleTerm
    = { name = "MyTypes"
      , bindings =
        [ opaque "Integer" emptyArgs
        , opaque "Bool" emptyArgs
        , opaque "Text" emptyArgs
        , opaque "Set" [ "a" ]
        , opaque "List" [ "a" ]
        , opaque "Map" [ "k", "v" ]
        , data "Unit" emptyArgs (toMap { Unit = nullary })
        , data "Maybe" [ "a" ] (toMap { Nothing = nullary, Just = var "a" })
        , let maybeInt = app "Maybe" [ "Integer" ]

          in  data "MyType" emptyArgs (toMap { MyConstr = maybeInt })
        ]
      , constraints = Prelude.List.empty Text
      }

in  mymod
