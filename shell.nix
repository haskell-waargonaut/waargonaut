{ nixpkgs ? import <nixpkgs> {}
  # pkgPath ? <nixpkgs>
, compiler ? "default"
, bench ? false
}:
(import ./default.nix { inherit nixpkgs compiler bench; }).env
