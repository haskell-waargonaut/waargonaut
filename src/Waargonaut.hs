{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
--
{-# LANGUAGE BangPatterns #-}
module Waargonaut where

import           Prelude                     (Eq, Show)

import           Control.Applicative         ((<$>), (<*), (<*>), (<|>))
import           Control.Category            ((.))
import           Control.Monad               (Monad)

import           Data.Distributive           (distribute)

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB

import           Data.Bool                   (Bool (..))

import           Data.Foldable               (Foldable (..), asum)

import           Data.Functor                (Functor (..))

import           Data.Semigroup              ((<>))

import           Data.Traversable            (Traversable (..))

import           Text.Parser.Char            (CharParsing, char, text)

import           Data.Digit                  (Digit, HeXaDeCiMaL)

import           Waargonaut.Types.CommaSep   (CommaSeparated (..),
                                              commaSeparatedBuilder,
                                              parseCommaSeparated)
import           Waargonaut.Types.JNumber
import           Waargonaut.Types.JString
import           Waargonaut.Types.Whitespace

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----

newtype JArray ws a =
  JArray (CommaSeparated ws a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- |
--
-- >>> testparse (parseJArray parseWhitespace simpleWaargonaut) "[null ]"
-- Right (JArray (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = Json (JNull (WS [Space])), _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJArray parseWhitespace simpleWaargonaut) "[null,]"
-- Right (JArray (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = Json (JNull (WS [])), _elemTrailing = Just (Comma,WS [])}}))))
--
parseJArray
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JArray ws a)
parseJArray ws a = JArray <$>
  parseCommaSeparated (char '[') (char ']') ws a

jArrayBuilder
  :: (WS -> Builder)
  -> JArray WS Json
  -> Builder
jArrayBuilder ws (JArray cs) =
  commaSeparatedBuilder '[' ']' ws (jsonBuilder ws) cs

data JsonAssoc digit ws a = JsonAssoc
  { _jsonAssocKey             :: JString digit
  , _jsonAssocKeyTrailingWS   :: !ws
  , _jsonAssocValPreceedingWS :: !ws
  , _jsonAssocVal             :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype JObject digit ws a =
  JObject (CommaSeparated ws (JsonAssoc digit ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

parseJsonAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JsonAssoc digit ws a)
parseJsonAssoc ws a = JsonAssoc
  <$> parseJString <*> ws <* char ':' <*> ws <*> a

jsonAssocBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonAssoc digit WS Json
  -> Builder
jsonAssocBuilder ws (JsonAssoc k ktws vpws v) =
  jStringBuilder k <> ws ktws <> BB.charUtf8 ':' <> ws vpws <> jsonBuilder ws v

-- |
--
-- >>> testparse (parseJObject parseWhitespace simpleWaargonaut) "{\"foo\":null }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = JsonAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS [Space]))}, _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJObject parseWhitespace simpleWaargonaut) "{\"foo\":null, }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = JsonAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS []))}, _elemTrailing = Just (Comma,WS [Space])}}))))
--
parseJObject
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JObject digit ws a)
parseJObject ws a = JObject <$>
  parseCommaSeparated (char '{') (char '}') ws (parseJsonAssoc ws a)

jObjectBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JObject digit WS Json
  -> Builder
jObjectBuilder ws (JObject c) =
  commaSeparatedBuilder '{' '}' ws (jsonAssocBuilder ws) c

data JTypes digit ws a
  = JNull ws
  | JBool Bool ws
  | JNum JNumber ws
  | JStr (JString digit) ws
  | JArr (JArray ws a) ws
  | JObj (JObject digit ws a) ws
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json
  = Json (JTypes Digit WS Json)
  deriving (Eq, Show)

jTypesBuilder
  :: (WS -> Builder)
  -> JTypes Digit WS Json
  -> BB.Builder
jTypesBuilder s (JNull tws)     = BB.stringUtf8 "null"                          <> s tws
jTypesBuilder s (JBool b tws)   = BB.stringUtf8 (if b then "true" else "false") <> s tws
jTypesBuilder s (JNum jn tws)   = jNumberBuilder jn                             <> s tws
jTypesBuilder s (JStr js tws)   = jStringBuilder js                             <> s tws
jTypesBuilder s (JArr js tws)   = jArrayBuilder s js                            <> s tws
jTypesBuilder s (JObj jobj tws) = jObjectBuilder s jobj                         <> s tws

jsonBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
jsonBuilder ws (Json jt) =
  jTypesBuilder ws jt

-- |
--
-- >>> testparse (parseJNull (return ())) "null"
-- Right (JNull ())
--
-- >>> testparsetheneof (parseJNull (return ())) "null"
-- Right (JNull ())
--
-- >>> testparsethennoteof (parseJNull (return ())) "nullx"
-- Right (JNull ())
--
parseJNull
  :: ( CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJNull ws = JNull
  <$ text "null"
  <*> ws

-- |
--
-- >>> testparse (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparse (parseJBool (return ())) "false"
-- Right (JBool False ())
---
-- >>> testparsetheneof (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparsetheneof (parseJBool (return ())) "false"
-- Right (JBool False ())
---
-- >>> testparsethennoteof (parseJBool (return ())) "truex"
-- Right (JBool True ())
--
-- >>> testparsethennoteof (parseJBool (return ())) "falsex"
-- Right (JBool False ())
--
parseJBool
  :: ( CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJBool ws =
  let
    b q t = JBool q <$ text t
  in
    (b False "false" <|> b True "true") <*> ws

parseJNum
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJNum ws =
  JNum <$> parseJNumber <*> ws

-- |
--
-- >>> testparse (parseJStr (return ())) "\"\""
-- Right (JStr (JString []) ())
--
-- >>> testparse (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >> testparse (parseJStr (return ())) "\"a\\rbc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (JTypes Digit () a)
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"\""
-- Right (JStr (JString []) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\\u"
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\t"
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJStr
  :: CharParsing f
  => f ws
  -> f (JTypes Digit ws a)
parseJStr ws =
  JStr <$> parseJString <*> ws

parseJArr
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJArr ws =
  JArr <$> parseJArray ws simpleWaargonaut <*> ws

parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJObj ws =
  JObj <$> parseJObject ws simpleWaargonaut <*> ws

parseWaargonaut
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseWaargonaut =
  asum . distribute
    [ parseJNull
    , parseJBool
    , parseJNum
    , parseJStr
    , parseJArr
    , parseJObj
    ]

simpleWaargonaut
  :: ( Monad f
     , CharParsing f
     )
  => f Json
simpleWaargonaut =
  Json <$> parseWaargonaut parseWhitespace
