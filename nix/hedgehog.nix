let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    hedgehog-pinned = initialNixpkgs.pkgs.lib.importJSON ./hedgehog.json;
    hedgehog = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "hedgehogqa";
      repo = "haskell-hedgehog";
      inherit (hedgehog-pinned) rev sha256;
    };
  };
in
  sources.hedgehog
