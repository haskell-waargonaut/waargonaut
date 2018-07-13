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

      digit          = self.callHackage "digit" "0.5.2" {};

      hoist-error    = self.callHackage "hoist-error" "0.2.1.0" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      tasty-hunit    = self.callHackage "tasty-hunit" "0.10.0.1" {};
    };
  };

  withBench = d:
    if bench then pkgs.haskell.lib.doBenchmark d else d;

  drv = withBench (
    modifiedHaskellPackages.callPackage ./waargonaut.nix {}
  );

in
  if pkgs.lib.inNixShell then drv.env else drv
