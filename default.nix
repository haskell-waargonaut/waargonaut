{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  pkgs = import nixpkgs {
    config.allowBroken = true;
    overlays = [(import ./waargonaut-deps.nix)];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
  pkgs.haskell.lib.shellAware drv
