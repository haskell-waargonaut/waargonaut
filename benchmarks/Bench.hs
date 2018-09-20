{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Gauge                 as G
import qualified Gauge.Main            as G

import qualified ConvenientFlatSurface as W

main :: IO ()
main = G.defaultMain
  [ G.bgroup "Parse File Only"
    [ G.bench "test1.json" $ G.nfIO (W.simpleParse "test1.json")
    , G.bench "test2.json" $ G.nfIO (W.simpleParse "test2.json")
    , G.bench "jp100"      $ G.nfIO (W.simpleParse "jp100.json")
    , G.bench "twitter100" $ G.nfIO (W.simpleParse "twitter100.json")
    , G.bench "numbers"    $ G.nfIO (W.simpleParse "numbers.json")
    ]
  ]
