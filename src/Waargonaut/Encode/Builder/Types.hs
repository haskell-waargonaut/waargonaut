-- |
--
-- The structure used to contain the required character and related functions for running a "builder".
--
module Waargonaut.Encode.Builder.Types (Builder (..)) where

-- | The builder data type.
data Builder t b = Builder
  { fromChar  :: Char -> b -- ^ Create a builder from a Haskell 'Char'
  , fromChunk :: t -> b -- ^ Create a builder from a chunk or piece of @t@
  , fromInt   :: Int -> b  -- ^ Create a builder from a Haskell 'Int'
  }
