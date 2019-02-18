module Waargonaut.Encode.Builder.Types (Builder (..)) where

data Builder t b = Builder
  { fromChar  :: Char -> b
  , fromChunk :: t -> b
  , fromInt   :: Int -> b
  }
