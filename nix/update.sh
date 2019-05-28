#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
nix-prefetch-git --no-deepClone https://github.com/NixOS/nixpkgs-channels refs/head/nixos-18.09 > nixpkgs.json
