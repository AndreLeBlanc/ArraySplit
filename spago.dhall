{ name = "ArraySplit"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "spec-quickcheck"
  , "test-unit"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
