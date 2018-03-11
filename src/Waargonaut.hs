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
--

module Waargonaut where

import           Numeric.Natural                  (Natural)

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Char8            as B8

import           Papa                             hiding (exp)
import           Prelude                          (error, maxBound, minBound)

import           Control.Applicative              as Applicative ((*>), (<*))
import           Control.Applicative              (Alternative (many, (<|>)))
import           Control.Monad                    ((=<<))
import           Data.Foldable                    (asum, length)

import           Data.Scientific                  (Scientific, scientific,
                                                   toDecimalDigits)
import           Data.Text                        as Text (Text, pack)

import           Text.Parser.Char
import           Text.Parser.Combinators

import           Data.Char                        (chr)

import           Data.Digit                       (Digit)
import qualified Data.Digit                       as Dig

import qualified Data.List.NonEmpty               as NE

import           Data.Digit.Decimal               (Decimal)
import           Data.Digit.HeXaDeCiMaL           (HeXaDeCiMaL)

import           Waargonaut.Types.JNumber         (JNumber)
import           Waargonaut.Types.JObject         (JObject)
import           Waargonaut.Types.JString         (JString)
import           Waargonaut.Types.LeadingTrailing (LeadingTrailing (..))

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative as Applicative((<*))
-- >>> import Data.Either(isLeft)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Text(pack)
-- >>> import Data.Text.Arbitrary
-- >>> import Data.Digit (Digit(..))
-- >>> import Text.Parsec(Parsec, ParseError, parse)
-- >>> import Test.QuickCheck (Arbitrary (..))
-- >>> instance Arbitrary Digit where arbitrary = Test.QuickCheck.elements [Digit1,Digit2,Digit3,Digit4,Digit5,Digit6,Digit7,Digit8,Digit9,Digit0]
-- >>> let testparse :: Parsec Text () a -> Text -> Either ParseError a; testparse p = parse p "test"
-- >>> let testparsetheneof :: Parsec Text () a -> Text -> Either ParseError a; testparsetheneof p = testparse (p <* eof)
-- >>> let testparsethennoteof :: Parsec Text () a -> Text -> Either ParseError a; testparsethennoteof p = testparse (p <* anyChar)
-- >>> let testparsethen :: Parsec Text () a -> Text -> Either ParseError (a, Char); testparsethen p = parse ((,) <$> p <*> Text.Parser.Char.anyChar) "test"

----

newtype Jsons digit s = Jsons
  { _jsonsL :: [LeadingTrailing (Json digit s) s]
  } deriving (Eq, Ord, Show)
makeClassy       ''Jsons
makeWrapped      ''Jsons

instance Functor (Jsons digit) where
    fmap f (Jsons ls) = Jsons (fmap ((\x -> x{_a = fmap f (_a x)}) . fmap f) ls)

instance Foldable (Jsons digit) where
    foldMap f (Jsons ls) = mconcat (fmap (foldMap f) ls)

instance Traversable (Jsons digit) where
    traverse f (Jsons ls) = Jsons <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

--  http://rfc7159.net/rfc7159
data Json digit s
  = JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString (JString digit) s
  | JsonArray (Jsons digit s) s
  | JsonObject (JObject digit s) s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeClassy       ''Json
makeClassyPrisms ''Json

jsonBuilder :: HeXaDeCiMaL digit => Json digit s -> BB.Builder
jsonBuilder (JsonNull _) = BB.byteString "null"
jsonBuilder (JsonBool b _) = BB.byteString $ if b then "true" else "false"
jsonBuilder (JsonNumber jn _) = jNumberBuilder jn
jsonBuilder (JsonString js _) = jStringBuilder js
jsonBuilder (JsonArray jsons _) = jsonsBuilder jsons
jsonBuilder (JsonObject jobj _) = jObjectBuilder jobj

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
