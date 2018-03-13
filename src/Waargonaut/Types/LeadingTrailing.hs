{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Waargonaut.Types.LeadingTrailing where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Data.Foldable           (asum)
import           Data.Functor            ((<$))

import           Data.Semigroup          ((<>))

import           Text.Parser.Char        (CharParsing, char, newline, tab)
import           Text.Parser.Combinators (many)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (Digit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Text.Parsec(ParseError)
-- >>> import Data.ByteString.Lazy (toStrict)
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> import Utils

data LeadingTrailing a s = LeadingTrailing
  { _leading  :: s
  , _a        :: a
  , _trailing :: s
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

parseLeadingTrailing ::
  Applicative f =>
  f s
  -> f a
  -> f (LeadingTrailing a s)
parseLeadingTrailing s a =
  LeadingTrailing <$> s <*> a <*> s

leadingTrailingBuilder
  :: (a -> Builder)
  -> (s -> Builder)
  -> LeadingTrailing a s
  -> Builder
leadingTrailingBuilder innerBuilder sBuilder lt =
  sBuilder (_leading lt) <> innerBuilder (_a lt) <> sBuilder (_trailing lt)


-- ws =
  -- %x20 /              ; Space
  -- %x09 /              ; Horizontal tab
  -- %x0A /              ; Line feed or New line
  -- %x0D )              ; Carriage return
data Whitespace
  = Space
  | HorizontalTab
  | LineFeed
  | NewLine
  | CarriageReturn
  deriving (Eq, Show)

newtype WS = WS [Whitespace]
  deriving (Eq, Show)

-- |
--
-- >>> testparse parseWhitespace " "
-- Right (WS [Space])
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
parseWhitespace
  :: CharParsing f
  => f WS
parseWhitespace =
  fmap WS . many $ asum
    [ Space          <$ char ' '
    , HorizontalTab  <$ tab
    , LineFeed       <$ char '\f'
    , CarriageReturn <$ char '\r'
    , NewLine        <$ newline
    ]

wsBuilder
  :: WS
  -> Builder
wsBuilder (WS ws) =
  foldMap wsB ws
  where
    wsB Space          = BB.charUtf8 ' '
    wsB HorizontalTab  = BB.charUtf8 '\t'
    wsB LineFeed       = BB.charUtf8 '\f'
    wsB CarriageReturn = BB.charUtf8 '\r'
    wsB NewLine        = BB.charUtf8 '\n'




