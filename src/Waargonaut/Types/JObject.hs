{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
--
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Waargonaut.Types.JObject
  ( JObject (..)
  , JAssoc (..)

  , HasJAssoc (..)
  , HasJObject (..)

  , jObjectBuilder
  , parseJObject
  ) where

import           Prelude                   (Eq, Show)

import           Control.Applicative       ((<*), (<*>))
import           Control.Category          (id, (.))
import           Control.Lens              (Lens')
import           Control.Monad             (Monad)

import           Data.Foldable             (Foldable)
import           Data.Functor              (Functor, fmap, (<$>))
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

data JAssoc digit ws a = JAssoc
  { _jsonAssocKey             :: JString digit
  , _jsonAssocKeyTrailingWS   :: ws
  , _jsonAssocValPreceedingWS :: ws
  , _jsonAssocVal             :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

class HasJAssoc c digit ws a | c -> digit ws a where
  jAssoc :: Lens' c (JAssoc digit ws a)
  jsonAssocKey :: Lens' c (JString digit)
  {-# INLINE jsonAssocKey #-}
  jsonAssocKeyTrailingWS :: Lens' c ws
  {-# INLINE jsonAssocKeyTrailingWS #-}
  jsonAssocVal :: Lens' c a
  {-# INLINE jsonAssocVal #-}
  jsonAssocValPreceedingWS :: Lens' c ws
  {-# INLINE jsonAssocValPreceedingWS #-}
  jsonAssocKey             = jAssoc . jsonAssocKey
  jsonAssocKeyTrailingWS   = jAssoc . jsonAssocKeyTrailingWS
  jsonAssocVal             = jAssoc . jsonAssocVal
  jsonAssocValPreceedingWS = jAssoc . jsonAssocValPreceedingWS

instance HasJAssoc (JAssoc digit ws a) digit ws a where
  jAssoc = id
  jsonAssocKey f (JAssoc x1 x2 x3 x4)             = fmap (\y1 -> JAssoc y1 x2 x3 x4) (f x1)
  {-# INLINE jsonAssocKey #-}
  jsonAssocKeyTrailingWS f (JAssoc x1 x2 x3 x4)   = fmap (\y1 -> JAssoc x1 y1 x3 x4) (f x2)
  {-# INLINE jsonAssocKeyTrailingWS #-}
  jsonAssocVal f (JAssoc x1 x2 x3 x4)             = fmap (\y1 -> JAssoc x1 x2 x3 y1) (f x4)
  {-# INLINE jsonAssocVal #-}
  jsonAssocValPreceedingWS f (JAssoc x1 x2 x3 x4) = fmap (\y1 -> JAssoc x1 x2 y1 x4) (f x3)
  {-# INLINE jsonAssocValPreceedingWS #-}

newtype JObject digit ws a =
  JObject (CommaSeparated ws (JAssoc digit ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

class HasJObject c digit ws a | c -> digit ws a where
  jObject :: Lens' c (JObject digit ws a)

instance HasJObject (JObject digit ws a) digit ws a where
  jObject = id

parseJAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JAssoc digit ws a)
parseJAssoc ws a = JAssoc
  <$> parseJString <*> ws <* char ':' <*> ws <*> a

jAssocBuilder
  :: HeXaDeCiMaL digit
  => (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JAssoc digit ws a
  -> Builder
jAssocBuilder ws aBuilder (JAssoc k ktws vpws v) =
  jStringBuilder k <> ws ktws <> BB.charUtf8 ':' <> ws vpws <> aBuilder ws v

-- |
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS [Space]))}, _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null, }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS []))}, _elemTrailing = Just (Comma,WS [Space])}}))))
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
