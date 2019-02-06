{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  # sigh = self: super: {
  #   haskellPackages = super.haskellPackages.override (old: {
  #     overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
  #       lens = hsuper.callHackage "lens" "4.17" {};
  #     });
  #   });
  # };

  pkgs = import nixpkgs {
    overlays = [ (import ./waargonaut-deps.nix) ];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./waargonaut.nix {};
in
  drv
