
let
  hostNix = import <nixpkgs> {};
  lensPin = hostNix.pkgs.lib.importJSON ./lens.json;

  lens = hostNix.pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo  = "lens";
    inherit (lensPin) rev sha256;
  };
in
  lens
