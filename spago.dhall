{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "geojson"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "effect"
    , "psci-support"
    , "web-url"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
