
let
  hostNix = import <nixpkgs> {};
  hw-mqueryPin = hostNix.pkgs.lib.importJSON ./hw-mquery.json;

  hw-mquery = hostNix.pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo  = "hw-mquery";
    inherit (hw-mqueryPin) rev sha256;
  };
in
  hw-mquery
