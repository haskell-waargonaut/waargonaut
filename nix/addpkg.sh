#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
PKG=$1
REPO=$2

NUX="
let
  hostNix = import <nixpkgs> {};
  ${PKG}Pin = hostNix.pkgs.lib.importJSON ./$PKG.json;

  $PKG = hostNix.pkgs.fetchFromGitHub {
    owner = \"$REPO\";
    repo  = \"$PKG\";
    inherit (${PKG}Pin) rev sha256;
  };
in
  $PKG"

nix-prefetch-git "https://github.com/$REPO/$PKG" > "$PKG.json"
echo "$NUX" > "$PKG.nix"

