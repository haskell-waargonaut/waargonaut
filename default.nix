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
    overrides = self: super: let
      sources = {
        digit = import ./nix/digit.nix;
      };
    in
    {
      digit          = self.callCabal2nix "digit" sources.digit {};

      hoist-error    = self.callHackage "hoist-error" "0.2.1.0" {};
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
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
