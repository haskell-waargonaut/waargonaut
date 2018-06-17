{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
module Waargonaut.Types.JString
  ( JString (..)
  , HasJString (..)
  , parseJString
  , jStringToText
  , jStringBuilder
  ) where

import           Prelude                 (Eq, Ord, Show)

import           Control.Applicative     ((*>), (<*))
import           Control.Category        (id, (.))
import           Control.Lens            (Lens', Rewrapped, Wrapped (..), iso)

import           Data.Digit              (HeXaDeCiMaL)
import           Data.Foldable           (Foldable, foldMap)
import           Data.Functor            (Functor, (<$>))
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Traversable        (Traversable)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many)

import qualified Data.ByteString.Builder as BB

import           Waargonaut.Types.JChar  (JChar, jCharBuilder, jCharToChar,
                                          parseJChar)

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
newtype JString digit =
  JString (Vector (JChar digit))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance JString digit ~ t => Rewrapped (JString digit) t

instance Wrapped (JString digit) where
  type Unwrapped (JString digit) = Vector (JChar digit)
  _Wrapped' = iso (\ (JString x) -> x) JString

class HasJString c digit | c -> digit where
  jString :: Lens' c (JString digit)

instance HasJString (JString digit) digit where
  jString = id

-- |
--
-- >>> testparse parseJString "\"\""
-- Right (JString [])
--
-- >>> testparse parseJString "\"\\\\\""
-- Right (JString [EscapedJChar ReverseSolidus])
--
-- >>> testparse parseJString "\"abc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >> testparse parseJString "\"a\\rbc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (JString Digit)
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark])
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
  char '"' *> (JString . V.fromList <$> many parseJChar) <* char '"'

-- | Convert a 'JString' to a strict 'Text'
--
-- >>> jStringToText (JString $ V.fromList [EscapedJChar (WhiteSpace HorizontalTab), UnescapedJChar (JCharUnescaped '1'), EscapedJChar (Hex (HexDigit4 Digit1 Digit2 Digit3 Digit4)), UnescapedJChar (JCharUnescaped '2')])
-- "\t1\4660\&2"
jStringToText :: HeXaDeCiMaL digit => JString digit -> Text
jStringToText (JString jcs) = foldMap (Text.singleton . jCharToChar) jcs

-- | jStringBuilder
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString V.empty) :: JString Digit)
-- "\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString Digit)
-- "\"abc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString Digit)
-- "\"a\\rbc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 Digita Digitb Digit1 Digit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) :: JString Digit)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString $ V.singleton (UnescapedJChar (JCharUnescaped 'a'))) :: JString Digit)
-- "\"a\""
--
-- >>> BB.toLazyByteString $ jStringBuilder (JString $ V.singleton (EscapedJChar ReverseSolidus) :: JString Digit)
-- "\"\\\\\""
--
jStringBuilder
  :: HeXaDeCiMaL digit
  => JString digit
  -> BB.Builder
jStringBuilder (JString jcs) =
  BB.charUtf8 '\"' <> foldMap jCharBuilder jcs <> BB.charUtf8 '\"'
