{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  pkgs = import nixpkgs {
    overlays = [(import ./waargonaut-deps.nix compiler)];
  };

  drv = pkgs.haskellPackages.callPackage ./waargonaut.nix {};
in
  pkgs.haskell.lib.shellAware drv
