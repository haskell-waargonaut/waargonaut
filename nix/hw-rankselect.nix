
let
  hostNix = import <nixpkgs> {};
  hw-rankselectPin = hostNix.pkgs.lib.importJSON ./hw-rankselect.json;

  hw-rankselect = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-rankselect";
    inherit (hw-rankselectPin) rev sha256;
  };
in
  hw-rankselect
