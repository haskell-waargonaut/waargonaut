{ nixpkgs ? import (import ./nix/nixpkgs.nix) {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (import ./waarg-hackage-overrides.nix pkgs.haskell.lib);
  });

  haskellPackages = baseHaskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
    (old.overrides or (_: _: {}))
    (import ./waarg-overrides.nix sources pkgs.haskell.lib);
  });

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
pkgs.haskell.lib.shellAware drv
