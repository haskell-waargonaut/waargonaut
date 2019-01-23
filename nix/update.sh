#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
nix-prefetch-git https://github.com/nixos/nixpkgs-channels refs/head/nixos-18.09 --no-deepClone > nixpkgs.json
