{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Types and functions for handling characters in JSON.
module Waargonaut.Types.JChar
  (
    -- * Types
    JChar (..)
  , AsJChar (..)
  , HasJChar (..)

    -- * Parser / Builder
  , parseJChar
  , jCharBuilderWith
  , jCharBuilderTextL
  , jCharBuilderByteStringL

    -- * Conversion
  , utf8CharToJChar
  , jCharToUtf8Char
  , jCharToChar
  , charToJChar
  ) where

import           Prelude                          (Char, Eq, Ord, Show,
                                                   otherwise, (/=))

import           Control.Category                 (id, (.))
import           Control.Lens                     (Lens', Prism', preview,
                                                   prism, review, ( # ))

import           Control.Applicative              ((<$>), (<|>))

import           Data.Bits                        ((.&.))
import           Data.Char                        (ord)
import           Data.Either                      (Either (..))
import           Data.Foldable                    (Foldable, asum, foldMap)
import           Data.Function                    (($))
import           Data.Functor                     (Functor)
import           Data.Maybe                       (Maybe (..), fromMaybe)
import           Data.Monoid                      (Monoid)
import           Data.Semigroup                   (Semigroup, (<>))
import           Data.Traversable                 (Traversable)

import qualified Data.Text.Internal               as Text

import           Data.Digit                       (HeXDigit, HeXaDeCiMaL)
import qualified Data.Digit                       as D

import           Data.Text.Lazy.Builder           (Builder)
import qualified Data.Text.Lazy.Builder           as TB

import qualified Data.ByteString.Lazy.Builder     as BB

import           Waargonaut.Types.Whitespace      (unescapedWhitespaceChar)

import           Text.Parser.Char                 (CharParsing)

import           Waargonaut.Types.JChar.HexDigit4 (HexDigit4 (..))

import           Waargonaut.Types.JChar.Escaped   (AsEscaped (..), Escaped (..),
                                                   charToEscaped, escapedToChar,
                                                   parseEscaped)

import           Waargonaut.Types.JChar.Unescaped (AsUnescaped (..), Unescaped,
                                                   parseUnescaped)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function (($))
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import Utils
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Waargonaut.Types.JChar.Unescaped
-- >>> import Waargonaut.Types.JChar.Escaped
-- >>> import Waargonaut.Types.JChar
----

-- | A JChar may be unescaped or escaped.
data JChar digit
  = EscapedJChar ( Escaped digit )
  | UnescapedJChar Unescaped
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Typeclass for things that have a 'JChar'.
class HasJChar c digit | c -> digit where
  jChar :: Lens' c (JChar digit)

instance HasJChar (JChar digit) digit where
  jChar = id

-- | Typeclass for things that be used as a 'JChar'.
class AsJChar r digit | r -> digit where
  _JChar          :: Prism' r (JChar digit)
  _EscapedJChar   :: Prism' r (Escaped digit)
  _UnescapedJChar :: Prism' r Unescaped

  _EscapedJChar   = _JChar . _EscapedJChar
  _UnescapedJChar = _JChar . _UnescapedJChar

instance AsJChar (JChar digit) digit where
  _JChar = id
  _EscapedJChar = prism EscapedJChar
    (\ x -> case x of
        EscapedJChar y1 -> Right y1
        _               -> Left x
    )
  _UnescapedJChar = prism UnescapedJChar
    (\ x -> case x of
        UnescapedJChar y1 -> Right y1
        _                 -> Left x
    )

instance AsEscaped (JChar digit) digit where
  _Escaped = _JChar . _Escaped

instance AsUnescaped (JChar digit) where
  _Unescaped = _JChar . _Unescaped

-- instance AsJChar Char HeXDigit where
-- Don't implement this, it's not a lawful prism.

jCharToChar :: JChar HeXDigit -> Char
jCharToChar (UnescapedJChar uejc) = review _Unescaped uejc
jCharToChar (EscapedJChar ejc)    = escapedToChar ejc

charToJChar :: Char -> Maybe (JChar HeXDigit)
charToJChar c =
  (UnescapedJChar <$> preview _Unescaped c) <|>
  (EscapedJChar <$> charToEscaped c)

utf8SafeChar :: Char -> Maybe Char
utf8SafeChar c | ord c .&. 0x1ff800 /= 0xd800 = Just c
               | otherwise                    = Nothing

-- | Convert a 'Char' to 'JChar HexDigit' and replace any invalid values with
-- @U+FFFD@ as per the 'Text' documentation.
--
-- Refer to <https://hackage.haskell.org/package/text/docs/Data-Text.html#g:2 'Text'> documentation for more info.
--
utf8CharToJChar :: Char -> JChar HeXDigit
utf8CharToJChar c = fromMaybe scalarReplacement (charToJChar $ Text.safe c)
  where scalarReplacement = EscapedJChar (Hex (HexDigit4 D.xf D.xf D.xf D.xd))
{-# INLINE utf8CharToJChar #-}

-- | Try to convert a 'JChar' to a 'Text' safe 'Char' value. Refer to the link for more info:
-- https://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text-Internal.html#v:safe
jCharToUtf8Char :: JChar HeXDigit -> Maybe Char
jCharToUtf8Char = utf8SafeChar . jCharToChar
{-# INLINE jCharToUtf8Char #-}

-- | Parse a JSON character.
--
-- >>> testparse parseJChar "\\u1234" :: Either DecodeError (JChar HeXDigit)
-- Right (EscapedJChar (Hex (HexDigit4 HeXDigit1 HeXDigit2 HeXDigit3 HeXDigit4)))
--
-- >>> testparse parseJChar "\\\\" :: Either DecodeError (JChar HeXDigit)
-- Right (EscapedJChar ReverseSolidus)
--
-- >>> testparse parseJChar "\\r"
-- Right (EscapedJChar (WhiteSpace CarriageReturn))
--
-- >>> testparsetheneof parseJChar "a"
-- Right (UnescapedJChar (Unescaped 'a'))
--
-- >>> testparsethennoteof parseJChar "ax"
-- Right (UnescapedJChar (Unescaped 'a'))
parseJChar ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JChar digit )
parseJChar = asum
  [ EscapedJChar <$> parseEscaped
  , UnescapedJChar <$> parseUnescaped
  ]

-- | Using the given function, return the builder for a single 'JChar'.
jCharBuilderWith
  :: ( Monoid builder
     , Semigroup builder
     , HeXaDeCiMaL digit
     )
  => (Char -> builder)
  -> JChar digit
  -> builder
jCharBuilderWith f (UnescapedJChar c) = f (review _Unescaped c)
jCharBuilderWith f (EscapedJChar jca) = f '\\' <> case jca of
    QuotationMark           -> f '"'
    ReverseSolidus          -> f '\\'
    Solidus                 -> f '/'
    Backspace               -> f 'b'
    (WhiteSpace ws)         -> f (unescapedWhitespaceChar ws)
    Hex (HexDigit4 a b c d) -> f 'u' <> foldMap hexChar [a,b,c,d]
  where
    hexChar =
      f . (D.charHeXaDeCiMaL #)

-- | Create a Lazy 'Text' 'Data.Text.Lazy.Builder' for the given 'JChar'.
jCharBuilderTextL :: HeXaDeCiMaL digit => JChar digit -> Builder
jCharBuilderTextL = jCharBuilderWith TB.singleton

-- | Create a Lazy 'ByteString' 'Data.ByteString.Lazy.Builder' for a given 'JChar'
jCharBuilderByteStringL :: HeXaDeCiMaL digit => JChar digit -> BB.Builder
jCharBuilderByteStringL = jCharBuilderWith BB.charUtf8
