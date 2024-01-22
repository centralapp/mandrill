let
  sources = import ./nix/sources.nix; 
  nixpkgs = import sources.nixpkgs {};
  inherit (nixpkgs) haskellPackages; 
  release = import ./release.nix {};
in haskellPackages.shellFor {
  packages = p: builtins.attrValues release;
  buildInputs = with haskellPackages; [
    cabal-install
    ghcid
  ];
}
