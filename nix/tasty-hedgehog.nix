let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    tasty-hedgehog-pinned = initialNixpkgs.pkgs.lib.importJSON ./tasty-hedgehog.json;
    tasty-hedgehog = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "tasty-hedgehog";
      inherit (tasty-hedgehog-pinned) rev sha256;
    };
  };
in
  sources.tasty-hedgehog