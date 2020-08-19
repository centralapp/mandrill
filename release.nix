let
  prelude = import ./prelude.nix;
  overlays = [
    (import ./overlay.nix)
  ];
  nixpkgs-overlayed = prelude.pkgs { inherit overlays; };

in { inherit (nixpkgs-overlayed.haskellPackages) mandrill; }
