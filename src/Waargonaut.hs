{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
-- | Top level module, reexporting the bare minimum of parsing and printing
-- functions.
module Waargonaut
  ( Json (..)
  , JType (..)
  , parseWaargonaut
  , waargonautBuilder
  ) where

import           Waargonaut.Types.Json                 (JType (..), Json (..),
                                                        parseWaargonaut,
                                                        waargonautBuilder)
