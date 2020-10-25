{ sources ? import ./nix/sources.nix
, compiler ? "default"
}:
let
  pkgs = import sources.nixpkgs {};

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
    (old.overrides or (_: _: {}))
    (import ./waarg-overrides.nix sources pkgs.haskell.lib);
  });

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
pkgs.haskell.lib.shellAware drv
