{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
--
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Types.JArray
  ( JArray (..)
  , parseJArray
  , jArrayBuilder
  )where

import           Prelude                   (Eq, Show)

import           Control.Lens              (Rewrapped, Wrapped (..), iso)
import           Control.Monad             (Monad)

import           Data.Foldable             (Foldable)
import           Data.Functor              (Functor, (<$>))
import           Data.Traversable          (Traversable)

import           Data.ByteString.Builder   (Builder)

import           Text.Parser.Char          (CharParsing, char)

import           Waargonaut.Types.CommaSep (CommaSeparated, commaSeparatedBuilder, parseCommaSeparated)

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

newtype JArray ws a =
  JArray (CommaSeparated ws a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance JArray ws a ~ t => Rewrapped (JArray ws a) t
instance Wrapped (JArray ws a) where
  type Unwrapped (JArray ws a) = CommaSeparated ws a
  _Wrapped' = iso (\(JArray x) -> x) JArray

-- |
--
-- >>> testparse (parseJArray parseWhitespace parseWaargonaut) "[null ]"
-- Right (JArray (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = Json (JNull (WS [Space])), _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJArray parseWhitespace parseWaargonaut) "[null,]"
-- Right (JArray (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = Json (JNull (WS [])), _elemTrailing = Just (Comma,WS [])}}))))
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
  :: (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JArray ws a
  -> Builder
jArrayBuilder ws a (JArray cs) =
  commaSeparatedBuilder '[' ']' ws (a ws) cs
