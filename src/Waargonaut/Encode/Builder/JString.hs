{-# LANGUAGE CPP #-}
-- |
--
-- Builder structures for 'JString's
--
module Waargonaut.Encode.Builder.JString (jStringBuilder) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                     ((<>))
#endif
import           Waargonaut.Types.JString        (JString, JString' (..))

import           Waargonaut.Encode.Builder.JChar (jCharBuilder)
import           Waargonaut.Encode.Builder.Types (Builder (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function (($))
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import qualified Data.Vector as V
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Waargonaut.Types.JChar.Unescaped
-- >>> import Waargonaut.Types.JChar.Escaped
-- >>> import Waargonaut.Types.JChar.HexDigit4
-- >>> import Waargonaut.Types.JChar
-- >>> import Waargonaut.Encode.Builder (textBuilder)
-- >>> import Data.Text.Lazy.Builder (toLazyText)
----

-- | Builder for a 'JString'.
--
-- >>> toLazyText $ jStringBuilder textBuilder ((JString' V.empty) :: JString)
-- "\"\""
--
-- >>> toLazyText $ jStringBuilder textBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')]) :: JString)
-- "\"abc\""
--
-- >>> toLazyText $ jStringBuilder textBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')]) :: JString)
-- "\"a\\rbc\""
--
-- >>> toLazyText $ jStringBuilder textBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (Unescaped 'd'),UnescapedJChar (Unescaped 'e'),UnescapedJChar (Unescaped 'f'),EscapedJChar QuotationMark]) :: JString)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> toLazyText $ jStringBuilder textBuilder ((JString' $ V.singleton (UnescapedJChar (Unescaped 'a'))) :: JString)
-- "\"a\""
--
-- >>> toLazyText $ jStringBuilder textBuilder (JString' $ V.singleton (EscapedJChar ReverseSolidus) :: JString)
-- "\"\\\\\""
--
jStringBuilder
  :: Monoid b
  => Builder t b
  -> JString
  -> b
jStringBuilder bldr (JString' jcs) =
  fromChar bldr '\"' <> foldMap (jCharBuilder bldr) jcs <> fromChar bldr '\"'
