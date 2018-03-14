{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE FunctionalDependencies #-}
--
module Waargonaut where

import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder          as BB

import           Prelude                          (Eq, Ord, Show)

import           Control.Applicative              (Alternative ((<|>)), (*>),
                                                   (<$>), (<*), (<*>))
import           Control.Category                 (id, (.))
import           Control.Lens                     (Lens', Prism', Rewrapped,
                                                   Wrapped (..), iso, prism)
import           Control.Monad                    (Monad)

import           Data.Bool                        (Bool (..))
import           Data.Either                      (Either (..))
import           Data.Foldable                    (Foldable (..), asum)
import           Data.Function                    (($))
import           Data.Functor                     (Functor (..))
import           Data.Semigroup                   (mconcat, (<>))
import           Data.Tuple                       (uncurry)

import           Data.Traversable                 (Traversable (..))

import           Text.Parser.Char                 (CharParsing, char, text)
import           Text.Parser.Combinators          (sepBy)

import           Data.Digit                       (Decimal, Digit, HeXaDeCiMaL)

import           Waargonaut.Types.JNumber         (JNumber, jNumberBuilder,
                                                   parseJNumber)
import           Waargonaut.Types.JString         (JString, jStringBuilder,
                                                   parseJString)

import           Waargonaut.Types.Whitespace      (WS, parseWhitespace)

import           Waargonaut.Types.LeadingTrailing (LeadingTrailing (..),
                                                   buildWrapped,
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

-- | Associated values, HashMap that cares about leading/trailing @Whitespace@
data JAssoc digit s = JAssoc
  { _key   :: LeadingTrailing (JString digit) s
  , _value :: LeadingTrailing (Json digit s) s
  }
  deriving (Eq, Ord, Show)

class HasJAssoc c digit s | c -> digit s where
  jAssoc :: Lens' c (JAssoc digit s)
  key :: Lens' c (LeadingTrailing (JString digit) s)
  {-# INLINE key #-}
  value :: Lens' c (LeadingTrailing (Json digit s) s)
  {-# INLINE value #-}

  key = jAssoc . key
  value = jAssoc . value

instance HasJAssoc (JAssoc digit s) digit s where
  jAssoc                 = id

  key f (JAssoc x1 x2)   = fmap (`JAssoc` x2) (f x1)
  {-# INLINE key #-}

  value f (JAssoc x1 x2) = fmap (JAssoc x1) (f x2)
  {-# INLINE value #-}

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

-- | JSON Array
newtype Jsons digit s = Jsons
  { _jsonsL :: [LeadingTrailing (Json digit s) s]
  } deriving (Eq, Ord, Show)

class HasJsons c digit s | c -> digit s where
  jsons :: Lens' c (Jsons digit s)
  jsonsL :: Lens' c [Waargonaut.Types.LeadingTrailing.LeadingTrailing (Json digit s) s]
  {-# INLINE jsonsL #-}
  jsonsL = (.) jsons jsonsL
instance HasJsons (Jsons digit s) digit s where
  {-# INLINE jsonsL #-}
  jsons = id
  jsonsL = iso (\ (Jsons x) -> x) Jsons

instance Jsons digit s ~ t => Rewrapped (Jsons digit s) t

instance Wrapped (Jsons digit s) where
  type Unwrapped (Jsons digit s) = [LeadingTrailing (Json digit s) s]
  _Wrapped' = iso (\ (Jsons x) -> x) Jsons

instance Functor (Jsons digit) where
    fmap f (Jsons ls) = Jsons (fmap ((\x -> x{_a = fmap f (_a x)}) . fmap f) ls)

instance Foldable (Jsons digit) where
    foldMap f (Jsons ls) = (foldMap . foldMap) f ls

instance Traversable (Jsons digit) where
    traverse f (Jsons ls) = Jsons <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

-- | JSON Object
newtype JObject digit s = JObject
  { _jobjectL :: [LeadingTrailing (JAssoc digit s) s]
  } deriving (Eq, Ord, Show)

instance Functor (JObject digit) where
    fmap f (JObject ls) = JObject (fmap fmap' ls) where
        fmap' (LeadingTrailing l x r) =
            LeadingTrailing (f l) (fmap f x) (f r)

instance Foldable (JObject digit) where
    foldMap f (JObject ls) = (foldMap . foldMap) f ls

instance Traversable (JObject digit) where
    traverse f (JObject ls) = JObject <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

class HasJObject c digit s | c -> digit s where
  jObject :: Lens' c (JObject digit s)
  jobjectL :: Lens' c [LeadingTrailing (JAssoc digit s) s]
  {-# INLINE jobjectL #-}
  jobjectL = jObject . jobjectL

instance HasJObject (JObject digit s) digit s where
  {-# INLINE jobjectL #-}
  jObject = id
  jobjectL = iso (\ (JObject x) -> x) JObject

instance JObject digit s ~ t => Rewrapped (JObject digit s) t

instance Wrapped (JObject digit s) where
  type Unwrapped (JObject digit s) = [LeadingTrailing (JAssoc digit s) s]
  _Wrapped' = iso (\ (JObject x) -> x) JObject

-- | Core JSON data structure, conforms to: <https://tools.ietf.org/html/rfc7159 rfc7159>
data Json digit s
  = JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString (JString digit) s
  | JsonArray (Jsons digit s) s
  | JsonObject (JObject digit s) s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class HasJson c digit s | c -> digit s where
  json :: Lens' c (Json digit s)

instance HasJson (Json digit s) digit s where
  json = id

class AsJson r digit s | r -> digit s where
  _Json       :: Prism' r (Json digit s)
  _JsonNull   :: Prism' r s
  _JsonBool   :: Prism' r (Bool, s)
  _JsonNumber :: Prism' r (Waargonaut.Types.JNumber.JNumber, s)
  _JsonString :: Prism' r (Waargonaut.Types.JString.JString digit, s)
  _JsonArray  :: Prism' r (Jsons digit s, s)
  _JsonObject :: Prism' r (JObject digit s, s)

  _JsonNull   = _Json . _JsonNull
  _JsonBool   = _Json . _JsonBool
  _JsonNumber = _Json . _JsonNumber
  _JsonString = _Json . _JsonString
  _JsonArray  = _Json . _JsonArray
  _JsonObject = _Json . _JsonObject

instance AsJson (Json digit s) digit s where
  _Json = id
  _JsonNull = prism JsonNull
    (\ x -> case x of
        JsonNull y1 -> Right y1
        _           -> Left x
    )

  _JsonBool = prism (uncurry JsonBool)
    (\ x -> case x of
        JsonBool y1 y2 -> Right (y1, y2)
        _              -> Left x
    )

  _JsonNumber = prism (uncurry JsonNumber)
    (\ x -> case x of
        JsonNumber y1 y2 -> Right (y1, y2)
        _                -> Left x
    )

  _JsonString = prism (uncurry JsonString)
    (\ x -> case x of
        JsonString y1 y2 -> Right (y1, y2)
        _                -> Left x
    )

  _JsonArray = prism (uncurry JsonArray)
    (\ x -> case x of
        JsonArray y1 y2 -> Right (y1, y2)
        _               -> Left x
    )

  _JsonObject = prism (uncurry JsonObject)
    (\ x -> case x of
        JsonObject y1 y2 -> Right (y1, y2)
        _                -> Left x
    )

parseJAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
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
  BB.charUtf8 ':' <>
  leadingTrailingBuilder (jsonBuilder sBuilder) sBuilder v

jsonsBuilder
  :: HeXaDeCiMaL digit
  => (s -> Builder)
  -> Jsons digit s
  -> Builder
jsonsBuilder sBuilder (Jsons jl) =
  buildWrapped '[' ']' sBuilder jsonBuilder jl

parseJObject
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
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
  buildWrapped '{' '}' sBuilder jAssocBuilder jL

jsonBuilder
  :: HeXaDeCiMaL digit
  => (s -> Builder)
  -> Json digit s
  -> BB.Builder
jsonBuilder s (JsonNull tws)        = BB.stringUtf8 "null"                          <> s tws
jsonBuilder s (JsonBool b tws)      = BB.stringUtf8 (if b then "true" else "false") <> s tws
jsonBuilder s (JsonNumber jn tws)   = jNumberBuilder jn                             <> s tws
jsonBuilder s (JsonString js tws)   = jStringBuilder js                             <> s tws
jsonBuilder s (JsonArray js tws)    = jsonsBuilder s js                             <> s tws
jsonBuilder s (JsonObject jobj tws) = jObjectBuilder s jobj                         <> s tws

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
parseJsonNull
  :: CharParsing f
  => f s
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
parseJsonBool
  :: CharParsing f
  => f s
  -> f (Json digit s)
parseJsonBool p =
  let
    b q t = JsonBool q <$ text t <*> p
  in
    b False "false" <|> b True "true"

parseJsonNumber
  :: ( Monad f
     , CharParsing f
     , Decimal digit
     )
  => f s
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
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparse (parseJsonString (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (Json Digit ())
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
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
parseJsonString
  :: ( CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
  -> f (Json digit s)
parseJsonString p =
  JsonString <$> parseJString <*> p

parseJsons
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
  -> f (Jsons digit s)
parseJsons s =
  Jsons <$>
    (
      char '[' *>
      sepBy (parseLeadingTrailing s (parseJson s)) (char ',') <*
      char ']'
    )

parseJsonArray
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
  -> f (Json digit s)
parseJsonArray p =
  JsonArray <$> parseJsons p <*> p

parseJsonObject
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
  -> f (Json digit s)
parseJsonObject p =
  JsonObject <$> parseJObject p <*> p

parseJson
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f s
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

simpleParseJson
  :: ( Monad f
     , CharParsing f
     )
  => f (Json Digit WS)
simpleParseJson =
  parseJson parseWhitespace
