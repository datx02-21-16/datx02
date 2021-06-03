{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unicode"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
