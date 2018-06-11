{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Types.Json
  ( JTypes (..)
  , Json (..)
  , waargonautBuilder
  , parseWaargonaut
  ) where

import           Prelude                     (Eq, Show)

import           Control.Applicative         ((<$>), (<*>), (<|>))
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

import           Text.Parser.Char            (CharParsing, text)

import           Data.Digit                  (Digit)

import           Waargonaut.Types.JArray     (JArray (..), jArrayBuilder,
                                              parseJArray)
import           Waargonaut.Types.JNumber    (JNumber, jNumberBuilder,
                                              parseJNumber)
import           Waargonaut.Types.JObject    (JObject, jObjectBuilder,
                                              parseJObject)
import           Waargonaut.Types.JString    (JString, jStringBuilder,
                                              parseJString)
import           Waargonaut.Types.Whitespace (WS, parseWhitespace)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----

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
jTypesBuilder s (JArr js tws)   = jArrayBuilder s waargonautBuilder js          <> s tws
jTypesBuilder s (JObj jobj tws) = jObjectBuilder s waargonautBuilder jobj       <> s tws

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
  JArr <$> parseJArray ws parseWaargonaut <*> ws

parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJObj ws =
  JObj <$> parseJObject ws parseWaargonaut <*> ws

parseJTypes
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJTypes =
  asum . distribute
    [ parseJNull
    , parseJBool
    , parseJNum
    , parseJStr
    , parseJArr
    , parseJObj
    ]

waargonautBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
waargonautBuilder ws (Json jt) =
  jTypesBuilder ws jt

parseWaargonaut
  :: ( Monad f
     , CharParsing f
     )
  => f Json
parseWaargonaut =
  Json <$> parseJTypes parseWhitespace
