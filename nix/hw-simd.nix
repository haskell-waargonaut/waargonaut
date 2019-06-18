
let
  hostNix = import <nixpkgs> {};
  hw-simdPin = hostNix.pkgs.lib.importJSON ./hw-simd.json;

  hw-simd = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-simd";
    inherit (hw-simdPin) rev sha256;
  };
in
  hw-simd
