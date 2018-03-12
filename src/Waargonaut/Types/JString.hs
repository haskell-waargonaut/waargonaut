{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Types.JString
  ( JString (..)
  , parseJString
  , jStringToText
  , jStringBuilder
  ) where

import           Prelude                 (Eq, Ord, Show)

import           Control.Applicative     ((*>), (<*))
import           Control.Category        ((.))
import           Control.Lens            (makeWrapped)

import           Data.Digit              (HeXaDeCiMaL)
import           Data.Foldable           (foldMap)
import           Data.Functor            ((<$>))

import           Data.Text               (Text)
import qualified Data.Text               as Text

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many)

import qualified Data.ByteString.Builder as BB

import           Waargonaut.Types.JChar  (JChar, jCharToChar, parseJChar)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (Digit(..))
-- >>> import Text.Parsec(ParseError)
-- >>> import Utils
-- >>> import Waargonaut.Types.JChar

----
newtype JString digit =
  JString [JChar digit]
  deriving (Eq, Ord, Show)
makeWrapped      ''JString

-- |
--
-- >>> testparse parseJString "\"\""
-- Right (JString [])
--
-- >>> testparse parseJString "\"abc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >> testparse parseJString "\"a\\rbc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (JString Digit)
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark])
--
-- >>> testparsetheneof parseJString "\"\""
-- Right (JString [])
--
-- >>> testparsetheneof parseJString "\"abc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparsethennoteof parseJString "\"a\"\\u"
-- Right (JString [UnescapedJChar (JCharUnescaped 'a')])
--
-- >>> testparsethennoteof parseJString "\"a\"\t"
-- Right (JString [UnescapedJChar (JCharUnescaped 'a')])
parseJString ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JString digit )
parseJString =
  char '"' *> (JString <$> many parseJChar) <* char '"'

-- | Convert a 'JString' to a strict 'Text'
--
-- >>> jStringToText (JString [EscapedJChar Tab, UnescapedJChar (JCharUnescaped '1'), EscapedJChar (Hex (HexDigit4 Digit1 Digit2 Digit3 Digit4)), UnescapedJChar (JCharUnescaped '2')])
-- "\t1\4660\&2"
jStringToText :: HeXaDeCiMaL digit => JString digit -> Text
jStringToText (JString jcs) = Text.pack (jCharToChar <$> jcs)

jStringBuilder
  :: HeXaDeCiMaL digit
  => JString digit
  -> BB.Builder
jStringBuilder (JString jcs) =
  foldMap (BB.char8 . jCharToChar) jcs
