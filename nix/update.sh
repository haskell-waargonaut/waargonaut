#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
nix-prefetch-git --no-deepClone https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-19.03 > nixpkgs.json
