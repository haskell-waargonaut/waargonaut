{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, bench ? false
}:
let
  # Can't use overlays as there is a infinite recursion in the list of
  # dependencies that needs to be fixed first.
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (import ./waargonaut-deps.nix);
  });

  withBench = d: if bench
    then pkgs.haskell.lib.doBenchmark d
    else d;

  drv = withBench (modifiedHaskellPackages.callPackage ./waargonaut.nix {});
in
  drv
