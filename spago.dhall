{ name = "ArraySplit"
, dependencies =
  [ "arrays"
  , "effect"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
