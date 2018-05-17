{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      digit = self.callHackage "digit" "0.5.2" {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./waargonaut.nix {};

in
  if pkgs.lib.inNixShell then drv.env else drv
