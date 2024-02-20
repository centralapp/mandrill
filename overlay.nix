args@
  { sources ? import ./nix/sources.nix
  , optimize ? false
  , haddocks ? false
  , failOnWarnings ? true
  , ...
  }:
self: super:
let
  inherit (super.haskell.lib)
    disableOptimization dontHaddock doJailbreak dontCheck appendConfigureFlag
    markUnbroken;
  inherit (super.lib) id pipe gitignore composeExtensions mapAttrs;
  doFailOnWarnings = x: appendConfigureFlag x "--ghc-option=-Werror";

  modPackage = package:
    pipe package [
      (if optimize then id else disableOptimization)
      (if haddocks then id else dontHaddock)
      (if failOnWarnings then doFailOnWarnings else id)
    ];

  overrides = selfh: superh: {
    mandrill = modPackage
      (superh.callCabal2nix "mandrill" (gitignore ./.) { });
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) overrides;
  });
}
