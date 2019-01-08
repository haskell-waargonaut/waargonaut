{ nixpkgsPath ? <nixpkgs>
, compiler ? "default"
}:
let
  pkgs = import nixpkgsPath {
    overlays = [ (import ./waargonaut-deps.nix) ];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
  drv
