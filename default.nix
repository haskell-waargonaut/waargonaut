{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, bench ? false
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

  withBench = d:
    if bench then pkgs.haskell.lib.doBenchmark d else d;

  drv = withBench (
    modifiedHaskellPackages.callPackage ./waargonaut.nix {}
  );

in
  if pkgs.lib.inNixShell then drv.env else drv
