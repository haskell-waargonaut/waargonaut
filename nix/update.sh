#! /usr/bin/env bash
nix-prefetch-git https://github.com/qfpl/digit > digit.json
nix-prefetch-git https://github.com/alephcloud/hs-hoist-error > hoist-error.json
nix-prefetch-git https://github.com/qfpl/tasty-hedgehog > tasty-hedgehog.json
# nix-prefetch-git https://github.com/feuerbach/tasty-hunit > tasty-hunit.json
# env NIX_REMOTE=daemon nix-prefetch-git https://github.com/mankyKitty/digit --rev 8901da98d2d156b37fdec59af9c449313f62e7ff > digit.json
