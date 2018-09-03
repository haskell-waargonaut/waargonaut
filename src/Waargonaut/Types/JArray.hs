{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-- | JSON Array representation and functions.
module Waargonaut.Types.JArray
  (
    -- * Types
    JArray (..)

    -- * Parser / Builder
  , parseJArray
  , jArrayBuilder
  ) where

import           Prelude                   (Eq, Show)

import           Control.Category          ((.))
import           Control.Error.Util        (note)
import           Control.Lens              (AsEmpty (..), Cons (..), Rewrapped,
                                            Wrapped (..), cons, isn't, iso,
                                            nearly, over, prism, to, ( # ),
                                            (^.), (^?), _2, _Wrapped)
import           Control.Monad             (Monad)

import           Data.Bifoldable           (Bifoldable (bifoldMap))
import           Data.Bifunctor            (Bifunctor (bimap))
import           Data.Bitraversable        (Bitraversable (bitraverse))
import           Data.Foldable             (Foldable)
import           Data.Function             (($))
import           Data.Functor              (Functor, (<$>))
import           Data.Monoid               (Monoid (..), mempty)
import           Data.Semigroup            (Semigroup (..))
import           Data.Traversable          (Traversable)

import           Data.ByteString.Builder   (Builder)

import           Text.Parser.Char          (CharParsing, char)

import           Waargonaut.Types.CommaSep (CommaSeparated,
                                            commaSeparatedBuilder,
                                            parseCommaSeparated)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
----

-- | Conveniently, a JSON array is a 'CommaSeparated' list with an optional
-- trailing comma, some instances and other functions need to work differently so
-- we wrap it up in a newtype.
newtype JArray ws a =
  JArray (CommaSeparated ws a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance JArray ws a ~ t => Rewrapped (JArray ws a) t
instance Wrapped (JArray ws a) where
  type Unwrapped (JArray ws a) = CommaSeparated ws a
  _Wrapped' = iso (\(JArray x) -> x) JArray

instance Monoid ws => Cons (JArray ws a) (JArray ws a) a a where
  _Cons = prism
    (\(a,j) -> over _Wrapped (cons a) j)
    (\j -> note j $ over _2 (_Wrapped #) <$> j ^? _Wrapped . _Cons)
  {-# INLINE _Cons #-}

instance (Semigroup ws, Monoid ws) => AsEmpty (JArray ws a) where
  _Empty = nearly (JArray mempty) (^. _Wrapped . to (isn't _Empty))
  {-# INLINE _Empty #-}

instance (Monoid ws, Semigroup ws) => Semigroup (JArray ws a) where
  (JArray a) <> (JArray b) = JArray (a <> b)

instance (Semigroup ws, Monoid ws) => Monoid (JArray ws a) where
  mempty = JArray mempty
  mappend = (<>)

instance Bifunctor JArray where
  bimap f g (JArray cs) = JArray (bimap f g cs)

instance Bifoldable JArray where
  bifoldMap f g (JArray cs) = bifoldMap f g cs

instance Bitraversable JArray where
  bitraverse f g (JArray cs) = JArray <$> bitraverse f g cs

-- | Parse a single JSON array
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

-- | Using the given builders, build a 'JArray'.
jArrayBuilder
  :: (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JArray ws a
  -> Builder
jArrayBuilder ws a (JArray cs) =
  commaSeparatedBuilder '[' ']' ws (a ws) cs
