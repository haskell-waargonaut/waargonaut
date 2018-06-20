{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Criterion.Main as C

import qualified ConvenientFlatSurface as W
import Waargonaut.Zipper (decodeTest1Json)

main :: IO ()
main = C.defaultMain
  [ C.bgroup "Parse File Only"
    [C.bench "test1.json" $ C.nfIO (W.simpleParse "test1.json")
    , C.bench "test2.json" $ C.nfIO (W.simpleParse "test2.json")
    , C.bench "jp100"      $ C.nfIO (W.simpleParse "jp100.json")
    , C.bench "twitter100" $ C.nfIO (W.simpleParse "twitter100.json")
    , C.bench "silly"      $ C.nfIO decodeTest1Json
    ]
  ]
