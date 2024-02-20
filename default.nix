args@
  { sources ? import ./nix/sources.nix
  , optimize ? false
  , haddocks ? false
  , failOnWarnings ? true
  , ...
  }:
(import ./release.nix args).mandrill
