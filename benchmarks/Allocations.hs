{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Weigh                 (Column (..), Weigh)
import qualified Weigh                 as W

import qualified ConvenientFlatSurface as C

simpleTests :: Weigh ()
simpleTests = W.wgroup "Simple Tests" $ do
  -- Run the allocations
  W.io "test1" C.simpleParse "test1.json"
  W.io "test2" C.simpleParse "test2.json"
  W.io "jp100" C.simpleParse "jp100.json"
  W.io "twitter100" C.simpleParse "twitter100.json"

main :: IO ()
main = W.mainWith $ do

  -- Setup some configuration
  W.setColumns [Case, Allocated, GCs, Live, Max, Check]

  -- Run the allocation examples
  simpleTests
