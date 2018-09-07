let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    hoist-error-pinned = initialNixpkgs.pkgs.lib.importJSON ./hoist-error.json;
    hoist-error = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "alephcloud";
      repo = "hs-hoist-error";
      inherit (hoist-error-pinned) rev sha256;
    };
  };
in
  sources.hoist-error