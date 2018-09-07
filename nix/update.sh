#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
nix-prefetch-git https://github.com/qfpl/digit > digit.json
nix-prefetch-git https://github.com/hedgehogqa/haskell-hedgehog > hedgehog.json
nix-prefetch-git https://github.com/qfpl/tasty-hedgehog > tasty-hedgehog.json
nix-prefetch-git https://github.com/feuerbach/tasty > tasty.json
nix-prefetch-git https://github.com/alephcloud/hs-hoist-error > hoist-error.json

