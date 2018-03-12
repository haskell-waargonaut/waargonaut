{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Waargonaut.Types.JChar where

import           Prelude                 (Char, Eq, Ord, Show, show, (&&), (*),
                                          (+), (<=), (==), (>=))

import           Control.Category        (id)
import           Control.Lens            (Lens', Prism', has, makeClassy,
                                          makeClassyPrisms, prism', ( # ))

import           Control.Applicative     (pure, (*>), (<$>), (<*>), (<|>))
import           Control.Monad           ((>>=))

import           Data.Char               (chr)
import           Data.Maybe              (Maybe (..))

import Data.Functor (($>))
import           Data.Foldable           (any, asum, foldl)

import           Data.Digit              (HeXaDeCiMaL)
import qualified Data.Digit              as D

import           Text.Parser.Char        (CharParsing, char, satisfy)
import           Text.Parser.Combinators (try)

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative as Applicative((<*))
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Text(pack)
-- >>> import Data.Text.Arbitrary
-- >>> import Data.Digit (Digit(..))
-- >>> import Data.Char (Char)
-- >>> import Text.Parsec(Parsec, ParseError, parse, eof, anyChar)
-- >>> import Test.QuickCheck (Arbitrary (..))
-- >>> instance Arbitrary Digit where arbitrary = Test.QuickCheck.elements [Digit1,Digit2,Digit3,Digit4,Digit5,Digit6,Digit7,Digit8,Digit9,Digit0]
-- >>> let testparse :: Parsec Text () a -> Text -> Either ParseError a; testparse p = parse p "test"
-- >>> let testparsetheneof :: Parsec Text () a -> Text -> Either ParseError a; testparsetheneof p = testparse (p <* eof)
-- >>> let testparsethennoteof :: Parsec Text () a -> Text -> Either ParseError a; testparsethennoteof p = testparse (p <* anyChar)
-- >>> let testparsethen :: Parsec Text () a -> Text -> Either ParseError (a, Char); testparsethen p = parse ((,) <$> p <*> Text.Parser.Char.anyChar) "test"

----
data HexDigit4 d =
  HexDigit4 d d d d
  deriving (Eq, Ord)
makeClassy       ''HexDigit4

instance Show d => Show ( HexDigit4 d ) where
  show (HexDigit4 q1 q2 q3 q4) =
    [
      q1
    , q2
    , q3
    , q4
    ] >>= show

newtype JCharUnescaped =
  JCharUnescaped Char
  deriving (Eq, Ord, Show)

class HasJCharUnescaped a where
  jCharUnescaped :: Lens' a JCharUnescaped

instance HasJCharUnescaped JCharUnescaped where
  jCharUnescaped = id

class AsJCharUnescaped a where
  _JCharUnescaped :: Prism' a JCharUnescaped

instance AsJCharUnescaped JCharUnescaped where
  _JCharUnescaped = id

instance AsJCharUnescaped Char where
  _JCharUnescaped = prism'
    (\(JCharUnescaped c) -> c)
    (\c ->  if any (\f -> f c) [(== '"'), (== '\\'), \x -> x >= '\x00' && x <= '\x1f']
            then Nothing
            else Just (JCharUnescaped c)
    )

data JCharEscaped digit
  = QuotationMark
  | ReverseSolidus
  | Solidus
  | Backspace
  | FormFeed
  | LineFeed
  | CarriageReturn
  | Tab
  | Hex ( HexDigit4 digit )
  deriving (Eq, Ord, Show)
makeClassy       ''JCharEscaped
makeClassyPrisms ''JCharEscaped

data JChar digit
  = EscapedJChar ( JCharEscaped digit )
  | UnescapedJChar JCharUnescaped
  deriving (Eq, Ord, Show)
makeClassy       ''JChar
makeClassyPrisms ''JChar

jCharToChar :: HeXaDeCiMaL digit => JChar digit -> Char
jCharToChar (UnescapedJChar (JCharUnescaped c)) = c
jCharToChar (EscapedJChar jca) = case jca of
    QuotationMark  -> '"'
    ReverseSolidus -> '\\'
    Solidus        -> '/'
    Backspace      -> '\b'
    FormFeed       -> '\f'
    LineFeed       -> '\n'
    CarriageReturn -> '\r'
    Tab            -> '\t'
    Hex (HexDigit4 a b c d) ->
      chr (foldl (\acc x -> 16*acc + (D.integralHexadecimal # x)) 0 [a,b,c,d])

-- |
--
-- >>> testparse parseHexDigit4 "1234" :: Either ParseError (HexDigit4 Digit)
-- Right 1234
--
-- >>> testparse parseHexDigit4 "12aF" :: Either ParseError (HexDigit4 Digit)
-- Right 12aF
--
-- >>> testparse parseHexDigit4 "aBcD" :: Either ParseError (HexDigit4 Digit)
-- Right aBcD
--
-- >>> testparsetheneof parseHexDigit4 "12aF" :: Either ParseError (HexDigit4 Digit)
-- Right 12aF
--
-- >>> testparsethennoteof parseHexDigit4 "12aFx" :: Either ParseError (HexDigit4 Digit)
-- Right 12aF
parseHexDigit4 ::
  ( CharParsing f, HeXaDeCiMaL digit ) =>
  f ( HexDigit4 digit )
parseHexDigit4 = HexDigit4
  <$> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL

-- |
--
-- >>> testparse parseJCharUnescaped "a"
-- Right (JCharUnescaped 'a')
--
-- >>> testparse parseJCharUnescaped "\8728"
-- Right (JCharUnescaped '\8728')
--
-- >>> testparsetheneof parseJCharUnescaped "a"
-- Right (JCharUnescaped 'a')
--
-- >>> testparsethennoteof parseJCharUnescaped "ax"
-- Right (JCharUnescaped 'a')
parseJCharUnescaped ::
  CharParsing f =>
  f JCharUnescaped
parseJCharUnescaped =
  JCharUnescaped <$> satisfy (has _JCharUnescaped)

-- |
--
-- >>> testparse parseJCharEscaped "\\\""
-- Right QuotationMark
--
-- >>> testparse parseJCharEscaped "\\\\"
-- Right ReverseSolidus
--
-- >>> testparse parseJCharEscaped "\\/"
-- Right Solidus
--
-- >>> testparse parseJCharEscaped "\\b"
-- Right Backspace
--
-- >>> testparse parseJCharEscaped "\\f"
-- Right FormFeed
--
-- >>> testparse parseJCharEscaped "\\n"
-- Right LineFeed
--
-- >>> testparse parseJCharEscaped "\\r"
-- Right CarriageReturn
--
-- >>> testparse parseJCharEscaped "\\t"
-- Right Tab
--
-- >>> testparse parseJCharEscaped "\\u1234" :: Either ParseError (JCharEscaped Digit)
-- Right (Hex 1234)
--
-- >>> testparsetheneof parseJCharEscaped "\\t"
-- Right Tab
--
-- >>> testparsethennoteof parseJCharEscaped "\\tx"
-- Right Tab
parseJCharEscaped ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JCharEscaped digit )
parseJCharEscaped =
  let z =
        asum
          ((\(c, p) -> char c *> pure p) <$>
            [
              ('"' , QuotationMark)
            , ('\\', ReverseSolidus)
            , ('/' , Solidus)
            , ('b' , Backspace)
            , ('f' , FormFeed)
            , ('n' , LineFeed)
            , ('r' , CarriageReturn)
            , ('t' , Tab)
            ])
      h =
        Hex <$> (char 'u' *> parseHexDigit4)
  in  char '\\' *> (z <|> h)

-- |
--
-- >>> testparse parseJChar "a"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
--
-- >>> testparse parseJChar "\\u1234" :: Either ParseError (JChar Digit)
-- Right (EscapedJChar (Hex 1234))
--
-- >>> testparse parseJChar "\\r"
-- Right (EscapedJChar CarriageReturn)
--
-- >>> testparsetheneof parseJChar "a"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
--
-- >>> testparsethennoteof parseJChar "ax"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
parseJChar ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JChar digit )
parseJChar =
  asum
    [
      EscapedJChar <$> try parseJCharEscaped
    , UnescapedJChar <$> parseJCharUnescaped
    ]
