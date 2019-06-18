
let
  hostNix = import <nixpkgs> {};
  generic-lensPin = hostNix.pkgs.lib.importJSON ./generic-lens.json;

  generic-lens = hostNix.pkgs.fetchFromGitHub {
    owner = "kcsongor";
    repo  = "generic-lens";
    inherit (generic-lensPin) rev sha256;
  };
in
  generic-lens
