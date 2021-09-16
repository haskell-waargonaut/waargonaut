{ sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs { overlays = [ (import ./nix/waargoverlay.nix) ]; };
in
  (pkgs.haskellPackages.callCabal2nix "waargonaut" ./. {}).env