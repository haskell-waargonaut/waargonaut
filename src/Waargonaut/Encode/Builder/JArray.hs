module Waargonaut.Encode.Builder.JArray (jArrayBuilder) where

import           Waargonaut.Types.JArray            (JArray (..))

import           Waargonaut.Encode.Builder.CommaSep (commaSeparatedBuilder)
import           Waargonaut.Encode.Builder.Types    (Builder)

-- | Using the given builders, build a 'JArray'.
jArrayBuilder
  :: Monoid b
  => Builder t b
  -> (Builder t b -> ws -> b)
  -> ((Builder t b -> ws -> b) -> Builder t b -> a -> b)
  -> JArray ws a
  -> b
jArrayBuilder bldr ws a (JArray cs) =
  commaSeparatedBuilder bldr '[' ']' ws (a ws) cs
