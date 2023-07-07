args@
  { sources ? import ./nix/sources.nix
  , optimize ? false
  , haddocks ? false
  , failOnWarnings ? true
  , ...
  }:
let
  ca-nixutils = import sources.ca-nixutils;

  overlays = (with ca-nixutils.overlays; [ utils ])
    ++ [ (import ./overlay.nix args) ];
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };

in { inherit (nixpkgs-overlayed.haskellPackages) mandrill; }
