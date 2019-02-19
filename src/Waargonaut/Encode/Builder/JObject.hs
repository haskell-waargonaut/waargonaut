module Waargonaut.Encode.Builder.JObject (jObjectBuilder) where

import           Data.Monoid                        ((<>))

import           Waargonaut.Types.JObject           (JAssoc (..), JObject (..))

import           Waargonaut.Encode.Builder.CommaSep (commaSeparatedBuilder)
import           Waargonaut.Encode.Builder.JString  (jStringBuilder)
import           Waargonaut.Encode.Builder.Types    (Builder (..))

-- | Builder for a single "key:value" pair.
jAssocBuilder
  :: Monoid b
  => (Builder t b -> ws -> b)
  -> ((Builder t b -> ws -> b) -> Builder t b -> a -> b)
  -> Builder t b
  -> JAssoc ws a
  -> b
jAssocBuilder ws aBuilder bldr (JAssoc k ktws vpws v) =
  jStringBuilder bldr k <> ws bldr ktws <> fromChar bldr ':' <> ws bldr vpws <> aBuilder ws bldr v

-- | Construct a 'Builder' for an entire 'JObject', duplicate keys are preserved.
jObjectBuilder
  :: Monoid b
  => Builder t b
  -> (Builder t b -> ws -> b)
  -> ((Builder t b -> ws -> b) -> Builder t b -> a -> b)
  -> JObject ws a
  -> b
jObjectBuilder bldr ws aBuilder (JObject c) =
  commaSeparatedBuilder bldr '{' '}' ws (jAssocBuilder ws aBuilder) c
