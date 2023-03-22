{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lambda-buffers-ctl"
, dependencies =
  [ "aeson"
  , "aff"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "maybe"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "spec"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
