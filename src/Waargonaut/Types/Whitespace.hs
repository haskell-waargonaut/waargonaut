{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Parsers and builders for whitespace characters in our JSON.
module Waargonaut.Types.Whitespace
  (
    Whitespace (..)
  , WS (..)
  , _WhitespaceChar

  , escapedWhitespaceChar
  , unescapedWhitespaceChar

  , oneWhitespace
  , parseWhitespace
  , parseSomeWhitespace

  , wsBuilder
  , wsRemover
  ) where

import           Control.Applicative     (liftA2)
import           Control.Lens            (AsEmpty (..), Cons (..), Prism',
                                          Rewrapped, Wrapped (..), iso,
                                          mapped, nearly, over, prism, prism',
                                          to, uncons, (^.), _2, _Wrapped)
import           Control.Lens.Extras     (is)

import           Data.Text.Lazy.Builder  (Builder)
import qualified Data.Text.Lazy.Builder  as TB

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.List.NonEmpty      (NonEmpty ((:|)))

import           Data.Foldable           (asum)
import           Data.Semigroup          (Semigroup (..))

import           Text.Parser.Char        (CharParsing, char, newline, tab)
import           Text.Parser.Combinators (many)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import qualified Data.Digit as D
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Data.ByteString.Lazy (toStrict)
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> import Utils
----

-- | Represent the different types of whitespace.
data Whitespace
  = Space
  | HorizontalTab
  | LineFeed
  | NewLine
  | CarriageReturn
  deriving (Eq, Ord, Show)

-- | This is a wrapper for a sequence of consecutive whitespace.
newtype WS = WS (Vector Whitespace)
  deriving (Eq, Show)

instance Cons WS WS Whitespace Whitespace where
  _Cons = prism' (\(w,ws) -> over _Wrapped (V.cons w) ws) (\(WS ws) -> over (mapped . _2) WS (uncons ws))
  {-# INLINE _Cons #-}

instance AsEmpty WS where
  _Empty = nearly mempty (^. _Wrapped . to (is _Empty))
  {-# INLINE _Empty #-}

instance WS ~ t => Rewrapped WS t
instance Wrapped WS where
  type Unwrapped WS = Vector Whitespace
  _Wrapped' = iso (\(WS x) -> x) WS
  {-# INLINE _Wrapped' #-}

instance Semigroup WS where
  (WS a) <> (WS b) = WS (a <> b)
  {-# INLINE (<>) #-}

instance Monoid WS where
  mempty = WS V.empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Handy 'Prism'' between a 'Char' its possible 'Whitespace' representation.
_WhitespaceChar :: Prism' Char Whitespace
_WhitespaceChar = prism escapedWhitespaceChar
  (\x -> case x of
      ' '  -> Right Space
      '\t' -> Right HorizontalTab
      '\f' -> Right LineFeed
      '\r' -> Right CarriageReturn
      '\n' -> Right NewLine
      _    -> Left x
      )

-- | Parse a single 'Whitespace' character.
oneWhitespace
  :: CharParsing f
  => f Whitespace
oneWhitespace = asum
  [ Space          <$ char ' '
  , HorizontalTab  <$ tab
  , LineFeed       <$ char '\f'
  , CarriageReturn <$ char '\r'
  , NewLine        <$ newline
  ]

-- |
--
-- >>> testparse parseWhitespace " "
-- Right (WS [Space])
--
-- >>> testparse parseWhitespace "\n    "
-- Right (WS [NewLine,Space,Space,Space,Space])
--
-- >>> testparse parseWhitespace " \t"
-- Right (WS [Space,HorizontalTab])
--
-- >>> testparse parseWhitespace "\f\f"
-- Right (WS [LineFeed,LineFeed])
--
-- >>> testparse parseWhitespace "\r\r\r"
-- Right (WS [CarriageReturn,CarriageReturn,CarriageReturn])
--
-- >>> testparse parseWhitespace "\n\r\r\n"
-- Right (WS [NewLine,CarriageReturn,CarriageReturn,NewLine])
--
-- >>> testparse parseWhitespace ""
-- Right (WS [])
--
-- >>> testparse parseWhitespace "\n   ]"
-- Right (WS [NewLine,Space,Space,Space])
--
parseWhitespace
  :: CharParsing f
  => f WS
parseWhitespace =
  WS . V.fromList <$> many oneWhitespace

-- | Parse a 'NonEmpty' sequence of consecutive whitespace.
parseSomeWhitespace
  :: CharParsing f
  => f (NonEmpty Whitespace)
parseSomeWhitespace =
  liftA2 (:|) oneWhitespace (many oneWhitespace)

-- | Change a 'Whitespace' into a single unescaped 'Char'. Useful if you're
-- already handling escaping with some other mechanism.
unescapedWhitespaceChar :: Whitespace -> Char
unescapedWhitespaceChar Space          = ' '
unescapedWhitespaceChar HorizontalTab  = 't'
unescapedWhitespaceChar LineFeed       = 'f'
unescapedWhitespaceChar CarriageReturn = 'r'
unescapedWhitespaceChar NewLine        = 'n'
{-# INLINE unescapedWhitespaceChar #-}

-- | Change a 'Whitespace' into its escaped 'Char' form.
escapedWhitespaceChar :: Whitespace -> Char
escapedWhitespaceChar Space          = ' '
escapedWhitespaceChar HorizontalTab  = '\t'
escapedWhitespaceChar LineFeed       = '\f'
escapedWhitespaceChar CarriageReturn = '\r'
escapedWhitespaceChar NewLine        = '\n'
{-# INLINE escapedWhitespaceChar #-}

-- | Create a 'Data.ByteString.Builder' from a 'Whitespace'
whitespaceBuilder :: Whitespace -> Builder
whitespaceBuilder Space          = TB.singleton ' '
whitespaceBuilder HorizontalTab  = TB.singleton '\t'
whitespaceBuilder LineFeed       = TB.singleton '\f'
whitespaceBuilder CarriageReturn = TB.singleton '\r'
whitespaceBuilder NewLine        = TB.singleton '\n'
{-# INLINE whitespaceBuilder #-}

-- | Reconstitute the given whitespace into its original form.
wsBuilder :: WS -> Builder
wsBuilder (WS ws) = foldMap whitespaceBuilder ws
{-# INLINE wsBuilder #-}

-- | Remove any whitespace. Minification for free, yay!
wsRemover :: WS -> Builder
wsRemover = const mempty
{-# INLINE wsRemover #-}
