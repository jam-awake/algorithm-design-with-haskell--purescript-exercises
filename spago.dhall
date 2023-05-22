{ name = "algorithm-design-with-haskell--purescript-exercises"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "partial"
  , "prelude"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "test/**/*.purs" ]
}
