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
    (import ./nix/waargoverlay.nix pkgs.haskell.lib);
  });

  drv = haskellPackages.callCabal2nix "waargonaut" ./. {};
in
pkgs.haskell.lib.shellAware drv
