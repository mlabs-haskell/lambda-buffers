{ name = "lbr-prelude"
, dependencies =
  [ "aeson"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "bigints"
  , "effect"
  , "either"
  , "encoding"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "quickcheck-utf8"
  , "spec"
  , "strings"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
