{ name = "lbr-plutus"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "js-bigints"
  , "cardano-transaction-lib"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "tuples"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
