{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

{-# LANGUAGE RankNTypes             #-}
--
{-# LANGUAGE GADTs                  #-}

module Waargonaut where

import Debug.Trace (traceShowId)

import           Papa                    hiding (exp)
import           Prelude                 (maxBound, minBound)

import           Control.Applicative     as Applicative ((*>), (<*))
import           Control.Applicative     (Alternative (many, (<|>)))
import           Data.Foldable           (asum)

import           Data.Scientific         (Scientific, scientific, toDecimalDigits)
import           Data.Text               as Text (Text, pack)
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Data.Char               (chr)
import           Data.List               (length)

import           Data.Digit              (Digit)
import qualified Data.Digit              as Dig

import qualified Data.List.NonEmpty      as NE

import           Data.Digit.Decimal      (Decimal)
import           Data.Digit.HeXaDeCiMaL  (HeXaDeCiMaL)

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative as Applicative((<*))
-- >>> import Data.Either(isLeft)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Text(pack)
-- >>> import Data.Text.Arbitrary
-- >>> import Data.Digit (Digit(Digit1,Digit2,Digit3,Digit4))
-- >>> import Text.Parsec(Parsec, ParseError, parse)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> let testparse :: Parsec Text () a -> Text -> Either ParseError a; testparse p = parse p "test"
-- >>> let testparsetheneof :: Parsec Text () a -> Text -> Either ParseError a; testparsetheneof p = testparse (p Applicative.<* eof)
-- >>> let testparsethennoteof :: Parsec Text () a -> Text -> Either ParseError a; testparsethennoteof p = testparse (p Applicative.<* anyChar)
-- >>> let testparsethen :: Parsec Text () a -> Text -> Either ParseError (a, Char); testparsethen p = parse ((,) <$> p <*> Text.Parser.Char.anyChar) "test"

----

data HexDigit4 d =
  HexDigit4 d d d d
  deriving (Eq, Ord)

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

data JChar digit
  = EscapedJChar ( JCharEscaped digit )
  | UnescapedJChar JCharUnescaped
  deriving (Eq, Ord, Show)

newtype JString digit =
  JString [JChar digit]
  deriving (Eq, Ord, Show)

data JInt
  = JZero
  | JIntInt (NonEmpty Digit)
  deriving (Eq, Ord, Show)

data E
  = EE
  | Ee
  deriving (Eq, Ord, Show)

newtype Frac = Frac (NonEmpty Digit)
  deriving (Eq, Ord, Show)

data Exp = Exp
  { _ex        :: E
  , _minusplus :: Maybe Bool
  , _expdigits :: NonEmpty Digit
  }
  deriving (Eq, Ord, Show)

data JNumber = JNumber
  { _minus     :: Bool
  , _numberint :: JInt
  , _frac      :: Maybe Frac
  , _expn      :: Maybe Exp
  }
  deriving (Eq, Ord, Show)

data JAssoc digit s = JAssoc
  { _key   :: LeadingTrailing ( JString digit ) s
  , _value :: LeadingTrailing (Json digit s) s
  }
  deriving (Eq, Ord, Show)

instance Functor ( JAssoc digit ) where
    fmap f (JAssoc k v) = JAssoc (fmap f k) ((\x -> x{_a = fmap f (_a x)}) . fmap f $ v)

instance Foldable ( JAssoc digit ) where
    foldMap f (JAssoc k v) = mconcat [foldMap f k, foldMap' v] where
        foldMap' (LeadingTrailing l x r) = mconcat [f l, foldMap f x, f r]

instance Traversable ( JAssoc digit ) where
    traverse f (JAssoc k v) = JAssoc <$> traverse f k <*> traverse' v where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

newtype JObject digit s = JObject
  { _jobjectL :: [LeadingTrailing (JAssoc digit s) s]
  } deriving (Eq, Ord, Show)

instance Functor ( JObject digit ) where
    fmap f (JObject ls) = JObject (fmap fmap' ls) where
        fmap' (LeadingTrailing l x r) =
            LeadingTrailing (f l) (fmap f x) (f r)

instance Foldable ( JObject digit ) where
    foldMap f (JObject ls) = mconcat (fmap (foldMap f) ls)

instance Traversable ( JObject digit ) where
    traverse f (JObject ls) = JObject <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

data LeadingTrailing a s = LeadingTrailing
  { _leading  :: s
  , _a        :: a
  , _trailing :: s
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

--  http://rfc7159.net/rfc7159
data Json digit s
  = JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString (JString digit) s
  | JsonArray (Jsons digit s) s
  | JsonObject (JObject digit s) s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Jsons digit s = Jsons
  { _jsonsL :: [LeadingTrailing (Json digit s) s]
  } deriving (Eq, Ord, Show)

instance Functor ( Jsons digit ) where
    fmap f (Jsons ls) = Jsons (fmap ((\x -> x{_a = fmap f (_a x)}) . fmap f) ls)

instance Foldable ( Jsons digit ) where
    foldMap f (Jsons ls) = mconcat (fmap (foldMap f) ls)

instance Traversable ( Jsons digit ) where
    traverse f (Jsons ls) = Jsons <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

makeClassy ''HexDigit4
makeClassy ''JCharEscaped
makeClassyPrisms ''JCharEscaped
makeClassy ''JChar
makeClassyPrisms ''JChar
makeWrapped ''JString
makeClassy ''JInt
makeClassyPrisms ''JInt
makeClassy ''E
makeClassyPrisms ''E
makeWrapped ''Frac
makeClassy ''Exp
makeClassy ''JNumber
makeClassy ''JAssoc
makeClassy ''JObject
makeWrapped ''JObject
makeClassy ''Json
makeClassyPrisms ''Json
makeClassy ''Jsons
makeWrapped ''Jsons

-- |
--
-- >>> testparse (parseJsonNull (return ())) "null"
-- Right (JsonNull ())
--
-- >>> testparsetheneof (parseJsonNull (return ())) "null"
-- Right (JsonNull ())
--
-- >>> testparsethennoteof (parseJsonNull (return ())) "nullx"
-- Right (JsonNull ())
--
-- prop> x /= "null" ==> isLeft (testparse (parseJsonNull (return ())) x)
parseJsonNull ::
  CharParsing f =>
  f s
  -> f (Json digit s)
parseJsonNull p =
  JsonNull <$ text "null" <*> p

-- |
--
-- >>> testparse (parseJsonBool (return ())) "true"
-- Right (JsonBool True ())
--
-- >>> testparse (parseJsonBool (return ())) "false"
-- Right (JsonBool False ())
---
-- >>> testparsetheneof (parseJsonBool (return ())) "true"
-- Right (JsonBool True ())
--
-- >>> testparsetheneof (parseJsonBool (return ())) "false"
-- Right (JsonBool False ())
---
-- >>> testparsethennoteof (parseJsonBool (return ())) "truex"
-- Right (JsonBool True ())
--
-- >>> testparsethennoteof (parseJsonBool (return ())) "falsex"
-- Right (JsonBool False ())
--
-- prop> (x `notElem` ["true", "false"]) ==> isLeft (testparse (parseJsonBool (return ())) x)
parseJsonBool ::
  CharParsing f =>
  f s
  -> f (Json digit s)
parseJsonBool p =
  let b q t = JsonBool q <$ text t <*> p
  in  b False "false" <|> b True "true"

parseJsonNumber ::
  (Monad f, CharParsing f, Decimal digit) =>
  f s
  -> f (Json digit s)
parseJsonNumber p =
  JsonNumber <$> parseJNumber <*> p

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
  <$> Dig.parseHeXaDeCiMaL
  <*> Dig.parseHeXaDeCiMaL
  <*> Dig.parseHeXaDeCiMaL
  <*> Dig.parseHeXaDeCiMaL

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
          ((\(c, p) -> char c Applicative.*> pure p) <$>
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
        Hex <$> (char 'u' Applicative.*> parseHexDigit4)
  in  char '\\' Applicative.*> (z <|> h)

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
  char '"' Applicative.*> (JString <$> many parseJChar) Applicative.<* char '"'

-- |
--
-- >>> testparse (parseJsonString (return ())) "\"\""
-- Right (JsonString (JString []) ())
--
-- >>> testparse (parseJsonString (return ())) "\"abc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >> testparse (parseJsonString (return ())) "\"a\\rbc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparse (parseJsonString (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (Json Digit ())
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJsonString (return ())) "\"\""
-- Right (JsonString (JString []) ())
--
-- >>> testparsetheneof (parseJsonString (return ())) "\"abc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJsonString (return ())) "\"a\"\\u"
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJsonString (return ())) "\"a\"\t"
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJsonString ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (Json digit s)
parseJsonString p =
  JsonString <$> parseJString <*> p

parseJsons ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (Jsons digit s)
parseJsons s =
  Jsons <$>
    (
      char '[' Applicative.*>
      sepBy (parseLeadingTrailing s (parseJson s)) (char ',') Applicative.<*
      char ']'
    )

parseJsonArray ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (Json digit s)
parseJsonArray p =
  JsonArray <$> parseJsons p <*> p

parseJAssoc ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JAssoc digit s)
parseJAssoc s =
  JAssoc <$> parseLeadingTrailing s parseJString Applicative.<* char ':' <*> parseLeadingTrailing s (parseJson s)

parseJObject ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JObject digit s)
parseJObject s =
  JObject <$>
    (
      char '{' Applicative.*>
      sepBy (parseLeadingTrailing s (parseJAssoc s)) (char ',') Applicative.<*
      char '}'
    )

parseJsonObject ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (Json digit s)
parseJsonObject p =
  JsonObject <$> parseJObject p <*> p

parseJson ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (Json digit s)
parseJson =
  asum . sequence
    [
      parseJsonNull
    , parseJsonBool
    , parseJsonNumber
    , parseJsonString
    , parseJsonArray
    , parseJsonObject
    ]

parseLeadingTrailing ::
  Applicative f =>
  f s
  -> f a
  -> f (LeadingTrailing a s)
parseLeadingTrailing s a =
  LeadingTrailing <$> s <*> a <*> s

----
-- |
--
-- >>> testparse parseJInt "1"
-- Right (JIntInt (1 :| []))
--
-- >>> testparse parseJInt "9"
-- Right (JIntInt (9 :| []))
--
-- >>> testparse parseJInt "10"
-- Right (JIntInt (1 :| [0]))
--
-- >>> testparse parseJInt "39"
-- Right (JIntInt (3 :| [9]))
--
-- >>> testparse parseJInt "393564"
-- Right (JIntInt (3 :| [9,3,5,6,4]))
--
-- >>> testparse parseJInt "0"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "00"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "01"
-- Right JZero
--
-- >>> testparsetheneof parseJInt "1"
-- Right (JIntInt (1 :| []))
--
-- >>> testparsetheneof parseJInt "9"
-- Right (JIntInt (9 :| []))
--
-- >>> testparsetheneof parseJInt "10"
-- Right (JIntInt (1 :| [0]))
--
-- >>> testparsetheneof parseJInt "39"
-- Right (JIntInt (3 :| [9]))
--
-- >>> testparsetheneof parseJInt "393564"
-- Right (JIntInt (3 :| [9,3,5,6,4]))
--
-- >>> testparsetheneof parseJInt "0"
-- Right JZero
--
-- >>> isLeft (testparse parseJInt "x")
-- True
--
-- >>> isLeft (testparse parseJInt "")
-- True
parseJInt ::
  (Monad f, CharParsing f) =>
  f JInt
parseJInt =
  asum [
    JZero <$ try (char '0')
  , fmap JIntInt $ ( NE.:| ) <$> Dig.parseDecimalNoZero <*> many Dig.parseDecimal
  ]

-- |
--
-- >>> testparse parseE "e"
-- Right Ee
--
-- >>> testparse parseE "E"
-- Right EE
--
-- >>> testparsetheneof parseE "e"
-- Right Ee
--
-- >>> testparsetheneof parseE "E"
-- Right EE
--
-- >>> isLeft (testparsetheneof parseE "x")
-- True
--
-- >>> testparsethennoteof parseE "ea"
-- Right Ee
--
-- >>> testparsethennoteof parseE "Ea"
-- Right EE
parseE ::
  CharParsing f =>
  f E
parseE =
  asum [
    Ee <$ try (char 'e')
  , EE <$ char 'E'
  ]

-- |
--
-- >>> testparsetheneof parseFrac "1"
-- Right (Frac (1 :| []))
--
-- >>> testparsetheneof parseFrac "9"
-- Right (Frac (9 :| []))
--
-- >>> testparsetheneof parseFrac "10"
-- Right (Frac (1 :| [0]))
--
-- >>> testparsetheneof parseFrac "39"
-- Right (Frac (3 :| [9]))
--
-- >>> testparsetheneof parseFrac "393564"
-- Right (Frac (3 :| [9,3,5,6,4]))
--
-- >>> testparsetheneof parseFrac "0"
-- Right (Frac (0 :| []))
--
-- >>> testparsetheneof parseFrac "00"
-- Right (Frac (0 :| [0]))
--
-- >>> testparsetheneof parseFrac "01"
-- Right (Frac (0 :| [1]))
--
-- >>> testparsethennoteof parseFrac "01x"
-- Right (Frac (0 :| [1]))
parseFrac ::
  (Monad f, CharParsing f) =>
  f Frac
parseFrac =
  Frac <$> some1 Dig.parseDecimal

-- |
--
-- >>> testparsethen parseExp "e10x"
-- Right (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e+10x"
-- Right (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e-0x"
-- Right (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| []},'x')
--
-- >>> testparsethen parseExp "E-1x"
-- Right (Exp {_ex = EE, _minusplus = Just True, _expdigits = 1 :| []},'x')
parseExp ::
  (Monad f, CharParsing f) =>
  f Exp
parseExp = Exp
  <$> parseE
  <*> optional (asum [False <$ char '+', True <$ char '-'])
  <*> some1 Dig.parseDecimal

-- |
--
-- >>> testparsethen parseJNumber "3x"
-- Right (JNumber {_minus = False, _numberint = JIntInt (3 :| []), _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3x"
-- Right (JNumber {_minus = True, _numberint = JIntInt (3 :| []), _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "0x"
-- Right (JNumber {_minus = False, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-0x"
-- Right (JNumber {_minus = True, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45x"
-- Right (JNumber {_minus = False, _numberint = JIntInt (3 :| []), _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3.45x"
-- Right (JNumber {_minus = True, _numberint = JIntInt (3 :| []), _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt (3 :| []), _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt (3 :| []), _frac = Nothing, _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3.45e+10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt (3 :| []), _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "-3.45e-02x"
-- Right (JNumber {_minus = True, _numberint = JIntInt (3 :| []), _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| [2]})},'x')
--
-- >>> isLeft (testparsethen parseJNumber "-3.45ex")
-- True
--
-- >>> isLeft (testparsethen parseJNumber "-.45e1x")
-- True
parseJNumber ::
  (Monad f, CharParsing f) =>
  f JNumber
parseJNumber = JNumber
  . isJust <$> optional (char '-')
  <*> parseJInt
  <*> optional (char '.' Applicative.*> parseFrac)
  <*> optional parseExp

-- | Returns a normalised 'Scientific' value or Nothing if the exponent
--   is out of the range @[minBound,maxBound::Int]@
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt ( Digit3 :| [] ), _frac = Just (Frac (Dig.x4 :| [Dig.x5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = Dig.x0 :| [Dig.x2]})}
-- Just -3.45e-2
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt (Dig.x1 :| [Dig.x2, Dig.x3]), _frac = Just (Frac (Dig.x4 :| [Dig.x5, Dig.x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = (Dig.x7 :| [Dig.x8, Dig.x9])})}
-- Just -1.23456e-787
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt (Dig.x1 :| [Dig.x2, Dig.x3]), _frac = Just (Frac (Dig.x4 :| [Dig.x5, Dig.x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = (Dig.x7 :| [Dig.x8, Dig.x9])})}
-- Just -1.23456e791
--
jNumberToScientific :: JNumber -> Maybe Scientific
jNumberToScientific (JNumber sign int mfrac mexp) =
    if expon > fromIntegral (maxBound :: Int) ||
       expon < fromIntegral (minBound :: Int)
         then Nothing
         else Just (scientific coeff (fromInteger expon))
    where
        intDigs       = jIntToDigits int

        fracList      = mfrac ^? _Just . _Wrapped
        exponentShift = fromMaybe 0 $ Data.List.length <$> fracList

        coeff         = neg (Just sign) (from _DigitNonEmpty # maybe intDigs (intDigs <>) fracList)

        expon         = maybe 0 expval mexp - fromIntegral exponentShift

        neg (Just True) = negate
        neg _           = id

        expval (Exp _ msign digs) = neg msign (from _DigitNonEmpty # digs)

jIntToInteger :: JInt -> Integer
jIntToInteger jsi = from _DigitNonEmpty # jIntToDigits jsi

jIntToDigits :: JInt -> NonEmpty Digit
jIntToDigits JZero        = Dig.x0 NE.:| []
jIntToDigits (JIntInt ds) = ds

-- | Convert a 'JString' to a strict 'Text'
--
-- >>> jStringToText (JString [EscapedJChar Tab, UnescapedJChar (JCharUnescaped '1'), EscapedJChar (Hex (HexDigit4 Digit1 Digit2 Digit3 Digit4)), UnescapedJChar (JCharUnescaped '2')])
-- "\t1\4660\&2"
jStringToText :: HeXaDeCiMaL digit => JString digit -> Text
jStringToText (JString jcs) = Text.pack (jCharToChar <$> jcs)

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
        chr (foldl (\acc x -> 16*acc + hexToInt x) 0 [a,b,c,d])

hexToInt :: HeXaDeCiMaL d => d -> Int
hexToInt = ( Dig.integralHexadecimal # )

-- | Iso between a NonEmpty Digit and an Integral number
--
-- >>> 1 ^. from _DigitNonEmpty
-- 1 :| []
--
-- >>> 0 ^. from _DigitNonEmpty
-- 0 :| []
--
-- >>> 9 ^. from _DigitNonEmpty
-- 9 :| []
--
-- >>> 393564 ^. from _DigitNonEmpty
-- 3 :| [9,3,5,6,4]
--
-- >>> (Dig.Digit1 :| []) ^. _DigitNonEmpty
-- 1
--
-- >>> (Dig.Digit0 :| []) ^. _DigitNonEmpty
-- 0
--
-- >>> (Dig.Digit9 :| []) ^. _DigitNonEmpty
-- 9
--
-- >>> (Dig.Digit3 :| [Dig.Digit9,Dig.Digit3,Dig.Digit5,Dig.Digit6,Dig.Digit4]) ^. _DigitNonEmpty
-- 393564
--
_DigitNonEmpty :: Integral n => Iso' (NonEmpty Digit) n
_DigitNonEmpty = iso integralFromDigits getDecimalDigits
  where
    getDecimalDigits 0 = Dig.Digit0 :| []
    getDecimalDigits n =
      let
        (dg,eXP) = toDecimalDigits $ fromIntegral (abs n)
        t = if length dg /= eXP
            then replicate (eXP - 1) Dig.Digit0
            else []
      in
        -- EWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW!
        NE.fromList . fromMaybe [Dig.Digit0] $ (<> t) <$> traverse (^? Dig.integralDecimal) dg

    integralFromDigits =
      snd . foldr (\d (i,n) -> (i + (1::Int), n + fromDigs d i)) base

    fromDigs dig ex' = (Dig.integralDecimal # dig) * (10 ^ ex')

    base = (0,0)

-- _DigitList :: Integral n => Prism' [Digit] n
-- _DigitList = prism' getDecimalDigits integralFromDigits
--   where
--     getDecimalDigits 0 = [Dig.Digit0]
--     getDecimalDigits n = reverse $ unfoldr go (abs n)
--       where
--         go 1 = Just (Dig.Digit1, 0)
--         go i =
--           let
--             (r, d) = quotRem i 10
--           in
--             if r == 0 then Nothing
--             else (,) <$> (d ^? Dig.integralDecimal) <*> pure r

--     integralFromDigits [] =
--       Nothing
--     integralFromDigits ds =
--       Just . snd $ foldl (\(i,n) d -> (i+(1::Int), n + fromDigs d i)) base ds

--     fromDigs dig ex' = (Dig.integralDecimal # dig) * (10 ^ ex')

--     base = (0,0)
