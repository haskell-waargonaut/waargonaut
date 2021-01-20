{ sources ? import ./../nix/sources.nix
, compiler ? "default"
}:
let
  pkgs = import sources.nixpkgs {};

  pkgs = import nixpkgs {
    config.allowBroken = true;
  };

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  withWaarg = baseHaskellPackages.override (old: {
    overrides = builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
    [ (import ../waarg-hackage-overrides.nix pkgs.haskell.lib)
      (hself: hsuper: {
        waargonaut = hself.callPackage ../waargonaut.nix {};
      })
    ];
  });

  drv = pkgs.haskell.lib.doBenchmark (withWaarg.callPackage ./waargbench.nix {});
in
  pkgs.haskell.lib.shellAware drv
