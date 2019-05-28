let
  hostNix = import <nixpkgs> {};
  hw-primPin = hostNix.pkgs.lib.importJSON ./hw-prim.json;

  hw-prim = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-prim";
    inherit (hw-primPin) rev sha256;
  };
in
  hw-prim
