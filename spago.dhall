{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "parsing"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
