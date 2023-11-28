{ name = "lbt-prelude"
, dependencies =
  [ "aeson"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "js-bigints"
  , "effect"
  , "either"
  , "encoding"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
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
, sources = [ "test/**/*.purs", ".extras/**/*.purs" ]
}
