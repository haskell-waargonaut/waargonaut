{ pkgs ? []
}:
let
  initialNixpkgs = import <nixpkgs> {};

  pkg-source = (owner: repo: pkg:
    let
      pkg-info-path = ./. + "/${pkg}.json";
    in
      if builtins.pathExists pkg-info-path
      then rec {
        pinned = initialNixpkgs.pkgs.lib.importJSON pkg-info-path;
        src = initialNixpkgs.pkgs.fetchFromGitHub {
          inherit owner repo;
          inherit (pinned) rev sha256;
        };
      }
      else throw "${pkg}.json doesn't exist!"
  );

in
  initialNixpkgs.lib.lists.foldl
    (acc: p: acc // {"${p.pkg}" = pkg-source p.owner p.repo p.pkg;})
    {}
    pkgs
