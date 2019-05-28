{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  upLens = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = hself: hsuper: {
        lens = hself.callHackage "lens" "4.17.1" {};
      };
    });
  };

  pkgs = import nixpkgs {
    overlays = [ (import ./waargonaut-deps.nix) ];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
  drv
