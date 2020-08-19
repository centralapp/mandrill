self: super:
let
  hlib = super.haskell.lib;
  lib = super.lib;
  # Filter the source based on the gitignore file. The path is the source path,
  # in which the gitignore should reside under `.gitignore`.
  gitignore = path:
    super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;

  mandrillOverrides = selfh: superh: {
    mandrill = hlib.dontHaddock
      (superh.callCabal2nix "mandrill" (gitignore ./.) { });
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { }))
      mandrillOverrides;
  });
}
