-- |
--
-- Builder structures for 'JChar's
--
module Waargonaut.Encode.Builder.JChar (jCharBuilder) where

import           Control.Lens                     (review)

import           Data.Monoid                      ((<>))

import           Data.Digit                       (HeXaDeCiMaL, charHeXaDeCiMaL)

import           Waargonaut.Encode.Builder.Types  (Builder (..))
import           Waargonaut.Types.JChar           (JChar (..))
import           Waargonaut.Types.JChar.Escaped   (Escaped (..))
import           Waargonaut.Types.JChar.HexDigit4 (HexDigit4 (..))
import           Waargonaut.Types.JChar.Unescaped (_Unescaped)
import           Waargonaut.Types.Whitespace      (unescapedWhitespaceChar)

-- | Using the given function, return the builder for a single 'JChar'.
jCharBuilder
  :: ( Monoid b
     , HeXaDeCiMaL digit
     )
  => Builder t b
  -> JChar digit
  -> b
jCharBuilder bldr (UnescapedJChar c) = fromChar bldr (review _Unescaped c)
jCharBuilder bldr (EscapedJChar jca) = fromChar bldr '\\' <> case jca of
    QuotationMark           -> fromChar bldr '"'
    ReverseSolidus          -> fromChar bldr '\\'
    Solidus                 -> fromChar bldr '/'
    Backspace               -> fromChar bldr 'b'
    (WhiteSpace ws)         -> fromChar bldr (unescapedWhitespaceChar ws)
    Hex (HexDigit4 a b c d) -> fromChar bldr 'u' <> foldMap hexChar [a,b,c,d]
  where
    hexChar =
      fromChar bldr . review charHeXaDeCiMaL
