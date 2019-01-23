{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
}:
(import ./default.nix) { inherit nixpkgs compiler; }
