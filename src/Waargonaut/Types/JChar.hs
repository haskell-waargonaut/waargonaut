{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Waargonaut.Types.JChar where

import           Papa                    (Bool (..), Char, Eq, Int, Ord, Show,
                                          fromIntegral, isJust, negate,
                                          replicate, show, some1, (&&), (*),
                                          (+), (-), (<), (<=), (==), (>), (>=),
                                          (^), (||))

import           Prelude                 (error, maxBound, minBound)

import           Data.Scientific         (Scientific)
import qualified Data.Scientific         as Sci
import           Numeric.Natural         (Natural)

import           Control.Category        (id, (.))
import           Control.Lens            (Lens', Prism', has, ifoldrM,
                                          makeClassy, makeClassyPrisms,
                                          makeWrapped, prism', to, ( # ), (^.),
                                          (^?), _Just, _Wrapped)

import           Control.Applicative     (pure, (*>), (<$), (<$>), (<*>), (<|>))
import           Control.Monad           (Monad, (>>=))

import           Data.Char               (chr)
import           Data.Function           (($))
import           Data.Functor            (fmap)
import           Data.Maybe              (Maybe (..), fromMaybe, maybe)
import           Data.Monoid             (mempty)
import           Data.Semigroup          ((<>))

import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.List.NonEmpty      as NE

import           Data.Foldable           (any, asum, foldMap, foldl, length)

import           Data.Digit              (Digit, HeXaDeCiMaL)
import qualified Data.Digit              as D

import           Text.Parser.Char        (CharParsing, char, satisfy)
import           Text.Parser.Combinators (many, optional, try)

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

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
