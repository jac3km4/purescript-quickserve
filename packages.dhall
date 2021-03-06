
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190306/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190306/src/packages.dhall sha256:eb8ae389eb218f1aad4c20054b8cce6c04a861a567aff72abd9111609178e986

let overrides = {=}

let additions =
  { query =
      mkPackage
      [ "prelude"
      , "record"
      , "tuples"
      , "arrays"
      , "integers"
      , "foreign-object"
      ]
      "https://github.com/jac3km4/purescript-query.git"
      "v0.0.2"
  }

in  upstream ⫽ overrides ⫽ additions
