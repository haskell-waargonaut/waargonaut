{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Top level types and functions for Waargonaut 'Json' types.
module Waargonaut.Types.Json
  (
    -- * Inner JSON types
    JType (..)
  , AsJType (..)

    -- * Top level JSON type
  , Json (..)
  , json

    -- * Parser / Builder
  , waargonautBuilder
  , parseWaargonaut
  ) where

import           Prelude                     (Eq, Show)

import           Control.Applicative         ((<$>), (<*>), (<|>))
import           Control.Category            (id, (.))
import           Control.Lens                (Prism', Rewrapped, Traversal',
                                              Wrapped (..), failing, iso, prism,
                                              traverseOf, _1, _Wrapped)
import           Control.Monad               (Monad)

import           Data.Bool                   (Bool (..))
import           Data.Distributive           (distribute)
import           Data.Either                 (Either (..))
import           Data.Foldable               (Foldable (..), asum)
import           Data.Functor                (Functor (..))
import           Data.Semigroup              ((<>))
import           Data.Traversable            (Traversable (..))
import           Data.Tuple                  (uncurry)

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB

import           Data.Digit                  (Digit)


import           Text.Parser.Char            (CharParsing, text)

import           Waargonaut.Types.JArray     (JArray (..), jArrayBuilder,
                                              parseJArray)
import           Waargonaut.Types.JNumber    (JNumber, jNumberBuilder,
                                              parseJNumber)
import           Waargonaut.Types.JObject    (JObject, jObjectBuilder,
                                              parseJObject)
import           Waargonaut.Types.JString    (JString, jStringBuilder,
                                              parseJString)
import           Waargonaut.Types.Whitespace (WS (..), parseWhitespace)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----

-- | Individual JSON Types and their trailing whitespace.
data JType digit ws a
  = JNull ws
  | JBool Bool ws
  | JNum JNumber ws
  | JStr JString ws
  | JArr (JArray ws a) ws
  | JObj (JObject ws a) ws
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Typeclass for things that can represent a 'JType'
class AsJType r digit ws a | r -> digit ws a where
  _JType :: Prism' r (JType digit ws a)
  _JNull  :: Prism' r ws
  _JBool  :: Prism' r (Bool, ws)
  _JNum   :: Prism' r (JNumber, ws)
  _JStr   :: Prism' r (JString, ws)
  _JArr   :: Prism' r (JArray ws a, ws)
  _JObj   :: Prism' r (JObject ws a, ws)

  _JNull = _JType . _JNull
  _JBool = _JType . _JBool
  _JNum  = _JType . _JNum
  _JStr  = _JType . _JStr
  _JArr  = _JType . _JArr
  _JObj  = _JType . _JObj

instance AsJType (JType digit ws a) digit ws a where
 _JType = id
 _JNull = prism JNull
       (\ x -> case x of
               JNull ws -> Right ws
               _        -> Left x
       )
 _JBool = prism (uncurry JBool)
       (\ x -> case x of
               JBool j ws -> Right (j, ws)
               _          -> Left x
       )
 _JNum = prism (uncurry JNum)
       (\ x -> case x of
               JNum j ws -> Right (j, ws)
               _         -> Left x
       )
 _JStr = prism (uncurry JStr)
       (\ x -> case x of
               JStr j ws -> Right (j, ws)
               _         -> Left x
       )
 _JArr = prism (uncurry JArr)
       (\ x -> case x of
               JArr j ws -> Right (j, ws)
               _         -> Left x
       )
 _JObj = prism (uncurry JObj)
       (\ x -> case x of
               JObj j ws -> Right (j, ws)
               _         -> Left x
       )

-- | Top level Json type, we specialise the whitespace to 'WS' and the 'digit'
-- type to 'Digit'. Also defining that our structures can recursively only contain
-- 'Json' types.
newtype Json
  = Json (JType Digit WS Json)
  deriving (Eq, Show)

instance Json ~ t => Rewrapped Json t
instance Wrapped Json where
  type Unwrapped Json = JType Digit WS Json
  _Wrapped' = iso (\(Json x) -> x) Json

-- | 'Json' is comprised of the different 'JType' types.
instance AsJType Json Digit WS Json where
  _JType = _Wrapped . _JType

-- | Ignoring whitespace, traverse a 'Json' structure.
json :: Traversal' Json Json
json = traverseOf (_Wrapped . failing (_JObj . _1 . traverse) (_JArr . _1 . _Wrapped . traverse))

-- | Using the provided whitespace builder, create a builder for a given 'JType'.
jTypesBuilder
  :: (WS -> Builder)
  -> JType Digit WS Json
  -> BB.Builder
jTypesBuilder s (JNull tws)     = BB.stringUtf8 "null"                          <> s tws
jTypesBuilder s (JBool b tws)   = BB.stringUtf8 (if b then "true" else "false") <> s tws
jTypesBuilder s (JNum jn tws)   = jNumberBuilder jn                             <> s tws
jTypesBuilder s (JStr js tws)   = jStringBuilder js                             <> s tws
jTypesBuilder s (JArr js tws)   = jArrayBuilder s waargonautBuilder js          <> s tws
jTypesBuilder s (JObj jobj tws) = jObjectBuilder s waargonautBuilder jobj       <> s tws

-- | Parse a 'null' value.
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
  -> f (JType Digit ws a)
parseJNull ws = JNull
  <$ text "null"
  <*> ws

-- | Parse a 'true' or 'false'.
--
-- >>> testparse (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparse (parseJBool (return ())) "false"
-- Right (JBool False ())
--
-- >>> testparsetheneof (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparsetheneof (parseJBool (return ())) "false"
-- Right (JBool False ())
--
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
  -> f (JType Digit ws a)
parseJBool ws =
  let
    b q t = JBool q <$ text t
  in
    (b False "false" <|> b True "true") <*> ws

-- | Parse a JSON numeric value.
parseJNum
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType Digit ws a)
parseJNum ws =
  JNum <$> parseJNumber <*> ws

-- | Parse a JSON string.
--
-- >>> testparse (parseJStr (return ())) "\"\""
-- Right (JStr (JString' []) ())
--
-- >>> testparse (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (JType Digit () a)
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"\""
-- Right (JStr (JString' []) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\\u"
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\t"
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJStr
  :: CharParsing f
  => f ws
  -> f (JType Digit ws a)
parseJStr ws =
  JStr <$> parseJString <*> ws

-- | Parse a JSON array.
parseJArr
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType Digit ws Json)
parseJArr ws =
  JArr <$> parseJArray ws parseWaargonaut <*> ws

-- | Parse a JSON object.
parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType Digit ws Json)
parseJObj ws =
  JObj <$> parseJObject ws parseWaargonaut <*> ws

-- | Try to parse each of our 'JType' possibilities.
parseJType
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType Digit ws Json)
parseJType =
  asum . distribute
    [ parseJNull
    , parseJBool
    , parseJNum
    , parseJStr
    , parseJArr
    , parseJObj
    ]

-- | Using the given whitespace builder, create a builder for a given 'Json' value.
waargonautBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
waargonautBuilder ws (Json jt) =
  jTypesBuilder ws jt

-- | Parse to a 'Json' value, keeping all of the information about the leading
-- and trailing whitespace.
parseWaargonaut
  :: ( Monad f
     , CharParsing f
     )
  => f Json
parseWaargonaut =
  Json <$> parseJType parseWhitespace
