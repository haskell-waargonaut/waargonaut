{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  overlay = self: super: 
    let
      baseHaskellPackages = if compiler == "default"
      then super.haskellPackages
      else super.haskell.packages.${compiler};
    in {
      haskellPackages = baseHaskellPackages.override (old: {
        overrides = super.lib.composeExtensions
          (old.overrides or (_: _: {}))
          (import ./waarg-hackage-overrides.nix super.haskell.lib);
      });
  };

  pkgs = import nixpkgs {
    overlays = [overlay];
  };

  drv = pkgs.haskellPackages.callPackage ./waargonaut.nix {};
in
  pkgs.haskell.lib.shellAware drv
