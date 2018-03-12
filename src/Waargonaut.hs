{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
--
module Waargonaut where

import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder          as BB

import           Prelude                          (Eq, Ord, Show)

import           Control.Applicative              (Alternative ((<|>)), (*>),
                                                   (<$>), (<*), (<*>))
import           Control.Category                 ((.))
import           Control.Monad                    (Monad)

import           Data.Bool                        (Bool (..))
import           Data.Foldable                    (Foldable (..), asum)
import           Data.Function                    (($))
import           Data.Functor                     (Functor (..))
import           Data.Semigroup                   (mconcat, (<>))

import           Data.Traversable                 (Traversable (..))

import           Text.Parser.Char                 (CharParsing, char, text)
import           Text.Parser.Combinators          (sepBy)

import           Data.Digit.Decimal               (Decimal)
import           Data.Digit.HeXaDeCiMaL           (HeXaDeCiMaL)

import           Waargonaut.Types.JNumber         (JNumber, jNumberBuilder,
                                                   parseJNumber)
import           Waargonaut.Types.JString         (JString, jStringBuilder,
                                                   parseJString)
import           Waargonaut.Types.LeadingTrailing (LeadingTrailing (..),
                                                   leadingTrailingBuilder,
                                                   parseLeadingTrailing)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----

data JAssoc digit s = JAssoc
  { _key   :: LeadingTrailing (JString digit) s
  , _value :: LeadingTrailing (Json digit s) s
  }
  deriving (Eq, Ord, Show)

instance Functor (JAssoc digit) where
    fmap f (JAssoc k v) = JAssoc (fmap f k) ((\x -> x{_a = fmap f (_a x)}) . fmap f $ v)

instance Foldable (JAssoc digit) where
    foldMap f (JAssoc k v) = mconcat [foldMap f k, foldMap' v] where
        foldMap' (LeadingTrailing l x r) = mconcat [f l, foldMap f x, f r]

instance Traversable (JAssoc digit) where
    traverse f (JAssoc k v) = JAssoc <$> traverse f k <*> traverse' v where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

parseJAssoc ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JAssoc digit s)
parseJAssoc s = JAssoc
  <$> parseLeadingTrailing s parseJString
  <* char ':'
  <*> parseLeadingTrailing s (parseJson s)

jAssocBuilder
  :: HeXaDeCiMaL digit
  => (s -> Builder)
  -> JAssoc digit s
  -> Builder
jAssocBuilder sBuilder (JAssoc k v) =
  leadingTrailingBuilder jStringBuilder sBuilder k <>
  BB.char8 ':' <>
  leadingTrailingBuilder (jsonBuilder sBuilder) sBuilder v

newtype Jsons digit s = Jsons
  { _jsonsL :: [LeadingTrailing (Json digit s) s]
  } deriving (Eq, Ord, Show)

jsonsBuilder
  ::HeXaDeCiMaL digit
  => (s -> Builder)
  -> Jsons digit s
  -> Builder
jsonsBuilder sBuilder (Jsons jl) =
  foldMap (leadingTrailingBuilder (jsonBuilder sBuilder) sBuilder) jl

newtype JObject digit s = JObject
  { _jobjectL :: [LeadingTrailing (JAssoc digit s) s]
  } deriving (Eq, Ord, Show)


instance Functor (JObject digit) where
    fmap f (JObject ls) = JObject (fmap fmap' ls) where
        fmap' (LeadingTrailing l x r) =
            LeadingTrailing (f l) (fmap f x) (f r)

instance Foldable (JObject digit) where
    foldMap f (JObject ls) = mconcat (fmap (foldMap f) ls)

instance Traversable (JObject digit) where
    traverse f (JObject ls) = JObject <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

parseJObject ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JObject digit s)
parseJObject s =
  JObject <$>
    (
      char '{' *>
      sepBy (parseLeadingTrailing s (parseJAssoc s)) (char ',') <*
      char '}'
    )

jObjectBuilder
  :: HeXaDeCiMaL digit
  => (s -> Builder)
  -> JObject digit s
  -> Builder
jObjectBuilder sBuilder (JObject jL) =
  BB.char8 '{' <>
  foldMap (leadingTrailingBuilder (jAssocBuilder sBuilder) sBuilder) jL <>
  BB.char8 '}'

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

jsonBuilder :: HeXaDeCiMaL digit => (s -> Builder) -> Json digit s -> BB.Builder
jsonBuilder _ (JsonNull _) = BB.byteString "null"
jsonBuilder _ (JsonBool b _) = BB.byteString $ if b then "true" else "false"
jsonBuilder _ (JsonNumber jn _) = jNumberBuilder jn
jsonBuilder _ (JsonString js _) = jStringBuilder js
jsonBuilder s (JsonArray jsons _) = jsonsBuilder s jsons
jsonBuilder s (JsonObject jobj _) = jObjectBuilder s jobj


-- makeClassy       ''JObject
-- makeWrapped      ''JObject

-- -- makeClassy       ''Json
-- makeClassyPrisms ''Json

-- makeClassy       ''JAssoc

-- makeClassy       ''Jsons
-- makeWrapped      ''Jsons

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
      char '[' *>
      sepBy (parseLeadingTrailing s (parseJson s)) (char ',') <*
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
