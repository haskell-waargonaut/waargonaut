let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    hedgehog-fn-pinned = initialNixpkgs.pkgs.lib.importJSON ./hedgehog-fn.json;
    hedgehog-fn = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "hedgehog-fn";
      inherit (hedgehog-fn-pinned) rev sha256;
    };
  };
in
  sources.hedgehog-fn
