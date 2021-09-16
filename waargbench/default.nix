{ sources ? import ./../nix/sources.nix
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
    (import ./../nix/waargoverlay.nix pkgs.haskell.lib);
  });

  withWaarg = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
      waargonaut = hself.callPackage ../waargonaut.nix {};
    });
  });

  drv = pkgs.haskell.lib.doBenchmark (withWaarg.callPackage ./waargbench.nix {});
in
  pkgs.haskell.lib.shellAware drv
