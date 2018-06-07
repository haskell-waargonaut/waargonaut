{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Criterion.Main as C

import           Control.Lens   (isn't, _Left)

import           Data.Semigroup ((<>))
import qualified Data.Text.IO   as TIO

import           Text.Parsec    (parse)

import qualified Waargonaut     as W

simpleParse
  :: FilePath
  -> IO Bool
simpleParse fp = isn't _Left
  . parse W.simpleWaargonaut "Simple Test"
  <$> TIO.readFile ("../test/json-data/" <> fp)

main :: IO ()
main = C.defaultMain
  [ C.bgroup "Parse File Only"
    [ C.bench "test1.json" $ C.nfIO (simpleParse "test1.json")
    , C.bench "test2.json" $ C.nfIO (simpleParse "test2.json")
    , C.bench "jp100"      $ C.nfIO (simpleParse "jp100.json")
    , C.bench "twitter100" $ C.nfIO (simpleParse "twitter100.json")
    ]
  ]
