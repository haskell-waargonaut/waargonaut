{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
}:
let

  pkgs = import nixpkgs {
    config.allowBroken = true;
    overlays = [ (import ../waargonaut-deps.nix) ];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  withWaarg = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
      waargonaut = hself.callPackage ../waargonaut.nix {};
    });
  });

  drv = pkgs.haskell.lib.doBenchmark (withWaarg.callPackage ./waargbench.nix {});
in
  pkgs.haskell.lib.shellAware drv
