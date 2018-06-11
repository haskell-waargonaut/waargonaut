module ConvenientFlatSurface
  ( simpleParse
  ) where

import           Control.Lens   (isn't, _Left)

import           Data.Semigroup ((<>))
import qualified Data.Text.IO   as TIO

import           Text.Parsec    (parse)

import qualified Waargonaut     as W

simpleParse
  :: FilePath
  -> IO Bool
simpleParse fp = isn't _Left
  . parse W.parseWaargonaut "Simple Test"
  <$> TIO.readFile ("../test/json-data/" <> fp)
