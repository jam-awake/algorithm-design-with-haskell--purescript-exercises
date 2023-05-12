{ name = "algorithm-design-with-haskell--purescript-exercises"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "spec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "test/**/*.purs" ]
}
