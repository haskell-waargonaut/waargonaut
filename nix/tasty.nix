let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    tasty-pinned = initialNixpkgs.pkgs.lib.importJSON ./tasty.json;
    tasty = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "feuerbach";
      repo = "tasty";
      inherit (tasty-pinned) rev sha256;
    };
  };
in
  sources.tasty
