{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
}:
let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = hself: hsuper: {
        waargonaut = hself.callPackage ../. {};
      };
    });
  };

  pkgs = import nixpkgs {
    overlays =
      [ (import ../waargonaut-deps.nix)
        overlay
      ];
  };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = pkgs.haskell.lib.doBenchmark (
    pkgs.haskellPackages.callPackage ./waargbench.nix {}
  );
in
  drv
