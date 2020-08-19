# import a pinned nixpkgs for use in this package.
let
  # src for our nixutils 
  # ca-nixutils-src = ../nixutils;
  ca-nixutils-src = builtins.fetchGit {
    url = "ssh://git@github.com/centralapp/ca-nixutils.git";
    rev = "892dbc753e8608ebf22dea5c9bec24b580c03686";
    ref = "develop";
  };
  ca-nixutils = import ca-nixutils-src;

  # Fetch the source of nixpkgs by a rev (short or long).
  # The short rev can be retrieved using:
  #   `nix-instantiate --eval -E '(import <nixpkgs> {}).lib.version'`
  pkgs = import (ca-nixutils.utils.fetchNixpkgsByRev "4b6bfecc0bd");
in { inherit ca-nixutils pkgs; }
