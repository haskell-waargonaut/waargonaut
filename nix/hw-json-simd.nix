let
  hostNix = import <nixpkgs> {};
  hw-json-simdPin = hostNix.pkgs.lib.importJSON ./hw-json-simd.json;

  hw-json-simd = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-json-simd";
    inherit (hw-json-simdPin) rev sha256;
  };
in
  hw-json-simd
