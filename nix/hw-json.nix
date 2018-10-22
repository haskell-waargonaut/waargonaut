let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    hw-json-pinned = initialNixpkgs.pkgs.lib.importJSON ./hw-json.json;
    hw-json = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "haskell-works";
      repo = "hw-json";
      inherit (hw-json-pinned) rev sha256;
    };
  };
in
  sources.hw-json