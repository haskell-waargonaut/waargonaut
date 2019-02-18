{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
-- | Types and functions for handling escaped characters in JSON.
module Waargonaut.Types.JChar.Escaped
  (
    -- * Types
    Escaped (..)
  , AsEscaped (..)

    -- * Parser
  , parseEscaped

    -- * Conversion
  , escapedToChar
  , charToEscaped
  ) where

import           Prelude                          (Eq, Ord, Show)

import           Control.Applicative              (pure, (*>), (<|>))
import           Control.Category                 (id, (.))

import           Control.Lens                     (Prism', preview, prism, to,
                                                   _Just)

import           Data.Foldable                    (Foldable, asum)
import           Data.Functor                     (Functor, (<$>))
import           Data.Traversable                 (Traversable)

import           Data.Function                    (const)

import           Data.Char                        (Char)
import           Data.Either                      (Either (..))
import           Data.Maybe                       (Maybe (..))

import           Data.Digit                       (HeXDigit, HeXaDeCiMaL)

import           Text.Parser.Char                 (CharParsing, char)

import           Waargonaut.Types.JChar.HexDigit4 (HexDigit4, charToHexDigit4,
                                                   hexDigit4ToChar,
                                                   parseHexDigit4)
import           Waargonaut.Types.Whitespace      (Whitespace (..),
                                                   unescapedWhitespaceChar,
                                                   _WhitespaceChar)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Utils
----

-- | Things that may be escaped in a JSON string.
data Escaped digit
  = QuotationMark
  | ReverseSolidus
  | Solidus
  | Backspace
  | WhiteSpace Whitespace
  | Hex ( HexDigit4 digit )
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Typeclass for things that may be used as an escaped JChar.
class AsEscaped r digit | r -> digit where
  _Escaped   :: Prism' r (Escaped digit)
  _QuotationMark  :: Prism' r ()
  _ReverseSolidus :: Prism' r ()
  _Solidus        :: Prism' r ()
  _Backspace      :: Prism' r ()
  _WhiteSpace     :: Prism' r Whitespace
  _Hex            :: Prism' r (HexDigit4 digit)

  _QuotationMark  = _Escaped . _QuotationMark
  _ReverseSolidus = _Escaped . _ReverseSolidus
  _Solidus        = _Escaped . _Solidus
  _Backspace      = _Escaped . _Backspace
  _WhiteSpace     = _Escaped . _WhiteSpace
  _Hex            = _Escaped . _Hex

instance AsEscaped (Escaped digit) digit where
  _Escaped = id
  _QuotationMark = prism (const QuotationMark)
    (\ x -> case x of
        QuotationMark -> Right ()
        _             -> Left x
    )
  _ReverseSolidus = prism (const ReverseSolidus)
    (\ x -> case x of
        ReverseSolidus -> Right ()
        _              -> Left x
    )
  _Solidus = prism (const Solidus)
    (\ x -> case x of
        Solidus -> Right ()
        _       -> Left x
    )
  _Backspace = prism (const Backspace)
    (\ x -> case x of
        Backspace -> Right ()
        _         -> Left x
    )
  _WhiteSpace = prism WhiteSpace
    (\ x -> case x of
        WhiteSpace y1 -> Right y1
        _             -> Left x
    )
  _Hex = prism Hex
    (\ x -> case x of
        Hex y1 -> Right y1
        _      -> Left x
    )

-- | Parse an escapted JSON character.
--
-- >>> testparse parseEscaped "\\\""
-- Right QuotationMark
--
-- >>> testparse parseEscaped "\\\\"
-- Right ReverseSolidus
--
-- >>> testparse parseEscaped "\\/"
-- Right Solidus
--
-- >>> testparse parseEscaped "\\b"
-- Right Backspace
--
-- >>> testparse parseEscaped "\\f"
-- Right (WhiteSpace LineFeed)
--
-- >>> testparse parseEscaped "\\n"
-- Right (WhiteSpace NewLine)
--
-- >>> testparse parseEscaped "\\r"
-- Right (WhiteSpace CarriageReturn)
--
-- >>> testparse parseEscaped "\\t"
-- Right (WhiteSpace HorizontalTab)
--
-- >>> testparse parseEscaped "\\u1234" :: Either DecodeError (Escaped HeXDigit)
-- Right (Hex (HexDigit4 HeXDigit1 HeXDigit2 HeXDigit3 HeXDigit4))
--
-- >>> testparsetheneof parseEscaped "\\t"
-- Right (WhiteSpace HorizontalTab)
--
-- >>> testparsethennoteof parseEscaped "\\tx"
-- Right (WhiteSpace HorizontalTab)
parseEscaped ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( Escaped digit )
parseEscaped =
  let
    z =
      asum ((\(c, p) -> char c *> pure p) <$>
        [
          ('"' , QuotationMark)
        , ('\\', ReverseSolidus)
        , ('/' , Solidus)
        , ('b' , Backspace)
        , (' ' , WhiteSpace Space)
        , ('f' , WhiteSpace LineFeed)
        , ('n' , WhiteSpace NewLine)
        , ('r' , WhiteSpace CarriageReturn)
        , ('t' , WhiteSpace HorizontalTab)
        ])
    h =
      Hex <$> (char 'u' *> parseHexDigit4)
  in
    char '\\' *> (z <|> h)

escapedToChar :: Escaped HeXDigit -> Char
escapedToChar = \case
  QuotationMark  -> '"'
  ReverseSolidus -> '\\'
  Solidus        -> '/'
  Backspace      -> '\b'
  WhiteSpace wc  -> unescapedWhitespaceChar wc
  Hex hd         -> hexDigit4ToChar hd

charToEscaped :: Char -> Maybe (Escaped HeXDigit)
charToEscaped c = case c of
  '"'  -> Just QuotationMark
  '\\' -> Just ReverseSolidus
  '/'  -> Just Solidus
  '\b' -> Just Backspace
  _    -> preview asWhitespace c <|> preview asHex c
  where
    asWhitespace = _WhitespaceChar . to WhiteSpace
    asHex = to charToHexDigit4 . _Just . to Hex
