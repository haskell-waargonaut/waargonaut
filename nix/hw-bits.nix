
let
  hostNix = import <nixpkgs> {};
  hw-bitsPin = hostNix.pkgs.lib.importJSON ./hw-bits.json;

  hw-bits = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-bits";
    inherit (hw-bitsPin) rev sha256;
  };
in
  hw-bits
