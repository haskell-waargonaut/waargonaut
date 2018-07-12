{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions for handling JSON strings.
module Waargonaut.Types.JString
  (
    -- * Types
    JString

    -- * Parser / Builder
  , parseJString
  , jStringBuilder

  , _JStringText
  ) where

import           Prelude                 (Eq, Ord, Show)

import           Control.Applicative     ((*>), (<*))
import           Control.Category        ((.))
import           Control.Error.Util      (note)
import           Control.Lens            (Prism', Rewrapped, Wrapped (..), iso,
                                          prism)

import           Data.Foldable           (Foldable, foldMap)
import           Data.Function           (($))
import           Data.Functor            (Functor, (<$>))
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Traversable        (Traversable, traverse)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Digit              (Digit)

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many)

import qualified Data.ByteString.Builder as BB

import           Waargonaut.Types.JChar  (JChar, jCharBuilder, jCharToUtf8Char,
                                          parseJChar, utf8CharToJChar)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((#))
-- >>> import Control.Monad (return)
-- >>> import Data.Function (($))
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (Digit(..))
-- >>> import qualified Data.Vector as V
-- >>> import Text.Parsec(ParseError)
-- >>> import Utils
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Waargonaut.Types.JChar
----

-- | A JSON string is a list of JSON acceptable characters, we use a newtype to
-- create the 'JString' type from a 'Vector JChar'. This is polymorphic over the
-- acceptable types of character encoding digits.
newtype JString' digit =
  JString' (Vector (JChar digit))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | As only one subset of digits are currently acceptable, Hexadecimal, we
-- provide this type alias to close that loop.
type JString = JString' Digit

instance JString' digit ~ t => Rewrapped (JString' digit) t

instance Wrapped (JString' digit) where
  type Unwrapped (JString' digit) = Vector (JChar digit)
  _Wrapped' = iso (\ (JString' x) -> x) JString'

-- | Parse a 'JString', storing escaped characters and any explicitly escaped
-- character encodings '\uXXXX'.
--
-- >>> testparse parseJString "\"\""
-- Right (JString' [])
--
-- >>> testparse parseJString "\"\\\\\""
-- Right (JString' [EscapedJChar ReverseSolidus])
--
-- >>> testparse parseJString "\"abc\""
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\""
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError JString
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark])
--
-- >>> testparsethennoteof parseJString "\"a\"\\u"
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a')])
--
-- >>> testparsethennoteof parseJString "\"a\"\t"
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a')])
parseJString
  :: CharParsing f
  => f JString
parseJString =
  char '"' *> (JString' . V.fromList <$> many parseJChar) <* char '"'

-- | Builder for a 'JString'.
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' V.empty) :: JString)
-- "\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"abc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"a\\rbc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 Digita Digitb Digit1 Digit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) :: JString)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.singleton (UnescapedJChar (JCharUnescaped 'a'))) :: JString)
-- "\"a\""
--
-- >>> BB.toLazyByteString $ jStringBuilder (JString' $ V.singleton (EscapedJChar ReverseSolidus) :: JString)
-- "\"\\\\\""
--
jStringBuilder
  :: JString
  -> BB.Builder
jStringBuilder (JString' jcs) =
  BB.charUtf8 '\"' <> foldMap jCharBuilder jcs <> BB.charUtf8 '\"'

-- | Prism between a 'JString' and 'Text'.
--
-- JSON strings a wider range of encodings than 'Text' and to be consistent with
-- the 'Text' type, these invalid types are replaced with a placeholder value.
--
_JStringText :: Prism' JString Text
_JStringText = prism
  (JString' . Text.foldr (V.cons . utf8CharToJChar) V.empty)
  (\j@(JString' v) -> note j $ Text.pack . V.toList <$> traverse jCharToUtf8Char v)
