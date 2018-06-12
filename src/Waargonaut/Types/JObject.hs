{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
--
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Waargonaut.Types.JObject
  ( JObject (..)
  , JAssoc (..)
  , JAssocKey (..)

  , HasJAssoc (..)
  , HasJObject (..)

  , jObjectBuilder
  , parseJObject
  ) where

import           Prelude                   (Eq, Show)

import           Control.Applicative       ((<*), (<*>))
import           Control.Lens              (makeClassy)
import           Control.Monad             (Monad)

import           Data.Foldable             (Foldable)
import           Data.Functor              (Functor, (<$>))
import           Data.Semigroup            ((<>))
import           Data.Traversable          (Traversable)

import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as BB

import           Text.Parser.Char          (CharParsing, char)

import           Data.Digit                (HeXaDeCiMaL)

import           Waargonaut.Types.CommaSep (CommaSeparated,
                                            commaSeparatedBuilder,
                                            parseCommaSeparated)

import           Waargonaut.Types.JString

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----
newtype JAssocKey digit = JAssocKey
  { unJAssocKey :: JString digit
  }
  deriving (Eq,Show)

data JAssoc digit ws a = JAssoc
  { _jsonAssocKey             :: JAssocKey digit
  , _jsonAssocKeyTrailingWS   :: ws
  , _jsonAssocValPreceedingWS :: ws
  , _jsonAssocVal             :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype JObject digit ws a =
  JObject (CommaSeparated ws (JAssoc digit ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

parseJAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JAssoc digit ws a)
parseJAssoc ws a = JAssoc
  <$> (JAssocKey <$> parseJString) <*> ws <* char ':' <*> ws <*> a

jAssocBuilder
  :: HeXaDeCiMaL digit
  => (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JAssoc digit ws a
  -> Builder
jAssocBuilder ws aBuilder (JAssoc (JAssocKey k) ktws vpws v) =
  jStringBuilder k <> ws ktws <> BB.charUtf8 ':' <> ws vpws <> aBuilder ws v

-- |
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS [Space]))}, _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null, }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS []))}, _elemTrailing = Just (Comma,WS [Space])}}))))
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
  parseCommaSeparated (char '{') (char '}') ws (parseJAssoc ws a)

jObjectBuilder
  :: HeXaDeCiMaL digit
  => (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JObject digit ws a
  -> Builder
jObjectBuilder ws aBuilder (JObject c) =
  commaSeparatedBuilder '{' '}' ws (jAssocBuilder ws aBuilder) c

makeClassy ''JAssoc
makeClassy ''JObject
