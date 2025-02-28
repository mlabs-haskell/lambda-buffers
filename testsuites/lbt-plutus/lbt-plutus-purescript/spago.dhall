{ name = "lbt-plutus"
, dependencies =
  [ "aeson"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "control"
  , "effect"
  , "either"
  , "encoding"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "js-bigints"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "plutus-types"
  , "prelude"
  , "quickcheck"
  , "quickcheck-utf8"
  , "rationals"
  , "spec"
  , "strings"
  , "tuples"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "test/**/*.purs", ".extras/**/*.purs" ]
}
