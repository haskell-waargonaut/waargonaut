let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    tasty-hunit-pinned = initialNixpkgs.pkgs.lib.importJSON ./tasty-hunit.json;
    tasty-hunit = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "feuerbach";
      repo = "tasty-hunit";
      inherit (tasty-hunit-pinned) rev sha256;
    };
  };
in
  sources.tasty-hunit