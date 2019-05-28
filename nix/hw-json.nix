let
  hostNix = import <nixpkgs> {};
  hw-jsonPin = hostNix.pkgs.lib.importJSON ./hw-json.json;

  hw-json = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-json";
    inherit (hw-jsonPin) rev sha256;
  };
in
  hw-json
