{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Types and functions for handling our representation of a JSON object.
module Waargonaut.Types.JObject
  (
    -- * Object Type
    JObject (..)
  , HasJObject (..)

    -- * Key/value pair type
  , JAssoc (..)
  , HasJAssoc (..)

    -- * Map-like object representation
  , MapLikeObj
  , toMapLikeObj
  , fromMapLikeObj

    -- * Parser / Builder
  , jObjectBuilder
  , parseJObject

    -- * Traversals
  , jassocWS
  , jobjectWS
  ) where

import           Prelude                   (Eq, Int, Show, elem, not, otherwise,
                                            (==))

import           Control.Applicative       ((<*), (<*>))
import           Control.Category          (id, (.))
import           Control.Lens              (AsEmpty (..), At (..), Index, traverseOf, re,
                                            IxValue, Ixed (..), Lens',
                                            Rewrapped, Traversal, Wrapped (..),
                                            cons, isn't, iso, nearly, to, ( # ),
                                            (.~), (<&>), (^.), (^?), _Wrapped)

import           Control.Monad             (Monad)
import           Data.Bool                 (Bool (..))
import           Data.Foldable             (Foldable, find, foldr)
import           Data.Function             (($))
import           Data.Functor              (Functor, fmap, (<$>))
import           Data.Maybe                (Maybe (..), maybe)
import           Data.Monoid               (Monoid (mempty))
import           Data.Semigroup            (Semigroup ((<>)))
import           Data.Text                 (Text)
import           Data.Traversable          (Traversable, traverse)

import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as BB

import qualified Data.Witherable           as W

import           Text.Parser.Char          (CharParsing, char)

import           Waargonaut.Types.CommaSep (CommaSeparated (..),
                                            commaSeparatedWS,
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
-- >>> import Data.Digit (HeXDigit)
----

-- | This type represents the key-value pair inside of a JSON object.
--
-- It is built like this so that we can preserve any whitespace information that
-- may surround it.
data JAssoc ws a = JAssoc
  { _jsonAssocKey             :: JString
  , _jsonAssocKeyTrailingWS   :: ws
  , _jsonAssocValPreceedingWS :: ws
  , _jsonAssocVal             :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

jassocWS :: Traversal a a' ws ws' -> Traversal (JAssoc ws a) (JAssoc ws' a') ws ws'
jassocWS g f (JAssoc k t p v) = JAssoc k <$> f t <*> f p <*> traverseOf g f v

-- | This class allows you to write connective lenses for other data structures
-- that may contain a 'JAssoc'.
class HasJAssoc c ws a | c -> ws a where
  jAssoc :: Lens' c (JAssoc ws a)
  jsonAssocKey :: Lens' c JString
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

instance HasJAssoc (JAssoc ws a) ws a where
  jAssoc = id
  jsonAssocKey f (JAssoc x1 x2 x3 x4)             = fmap (\y1 -> JAssoc y1 x2 x3 x4) (f x1)
  {-# INLINE jsonAssocKey #-}
  jsonAssocKeyTrailingWS f (JAssoc x1 x2 x3 x4)   = fmap (\y1 -> JAssoc x1 y1 x3 x4) (f x2)
  {-# INLINE jsonAssocKeyTrailingWS #-}
  jsonAssocVal f (JAssoc x1 x2 x3 x4)             = fmap (JAssoc x1 x2 x3) (f x4)
  {-# INLINE jsonAssocVal #-}
  jsonAssocValPreceedingWS f (JAssoc x1 x2 x3 x4) = fmap (\y1 -> JAssoc x1 x2 y1 x4) (f x3)
  {-# INLINE jsonAssocValPreceedingWS #-}

-- Helper function for trying to update/create a JAssoc value in some Functor.
-- This function is analogus to the 'Data.Map.alterF' function.
jAssocAlterF :: (Monoid ws, Functor f) => Text -> (Maybe a -> f (Maybe a)) -> Maybe (JAssoc ws a) -> f (Maybe (JAssoc ws a))
jAssocAlterF k f mja = fmap g <$> f (_jsonAssocVal <$> mja) where
  g v = maybe (JAssoc (textToJString k) mempty mempty v) (jsonAssocVal .~ v) mja

-- | The representation of a JSON object.
--
-- The <https://tools.ietf.org/html/rfc8259#section-4 JSON RFC8259> indicates
-- that names within an object "should" be unique. But the standard does not
-- enforce this, leaving it to the various implementations to decide how to
-- handle it.
--
-- As there are multiple possibilities for deciding which key to use when
-- enforcing uniqueness, Waargonaut accepts duplicate keys, allowing you to
-- decide how to handle it.
--
-- This type is the "list of tuples of key and value" structure, as such it is a
-- wrapper around the 'CommaSeparated' data type.
--
newtype JObject ws a =
  JObject (CommaSeparated ws (JAssoc ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

jobjectWS :: Traversal a a' ws ws' -> Traversal (JObject ws a) (JObject ws' a') ws ws'
jobjectWS g f (JObject cs) = JObject <$> commaSeparatedWS (jassocWS g) f cs

instance (Semigroup ws, Monoid ws) => AsEmpty (JObject ws a) where
  _Empty = nearly (_Wrapped # _Empty # ()) (^. _Wrapped . to (isn't _Empty))
  {-# INLINE _Empty #-}

instance JObject ws a ~ t => Rewrapped (JObject ws a) t

instance Wrapped (JObject ws a) where
  type Unwrapped (JObject ws a) = CommaSeparated ws (JAssoc ws a)
  _Wrapped' = iso (\ (JObject x) -> x) JObject

type instance IxValue (JObject ws a) = a
type instance Index (JObject ws a)   = Int

-- | Without having an obviously correct "first" or "last" decision on which
-- 'JString' key is the "right" one to use, a 'JObject' can only be indexed by a
-- numeric value.
instance Monoid ws => Ixed (JObject ws a) where
  ix i f (JObject cs) = JObject <$> ix i (traverse f) cs

-- | Type class to represent something that has a 'JObject' within it.
class HasJObject c ws a | c -> ws a where
  jObject :: Lens' c (JObject ws a)

instance HasJObject (JObject ws a) ws a where
  jObject = id

-- | This is a newtype around our 'JObject' for when we want to use the
-- "map-like" representation of our JSON object. This data type will enforce that
-- the first key found is treated as the desired element, and all subsequent
-- occurrences of that key are discarded.
newtype MapLikeObj ws a = MLO
  { fromMapLikeObj :: JObject ws a -- ^ Access the underlying 'JObject'.
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance MapLikeObj ws a ~ t => Rewrapped (MapLikeObj ws a) t

instance Wrapped (MapLikeObj ws a) where
  type Unwrapped (MapLikeObj ws a) = JObject ws a
  _Wrapped' = iso (\ (MLO x) -> x) MLO

instance (Monoid ws, Semigroup ws) => AsEmpty (MapLikeObj ws a) where
  _Empty = nearly (_Wrapped # _Empty # ()) (^. _Wrapped . to (isn't _Empty))
  {-# INLINE _Empty #-}

type instance IxValue (MapLikeObj ws a) = a
type instance Index (MapLikeObj ws a)   = Text

instance Monoid ws => Ixed (MapLikeObj ws a) where

-- | Unlike 'JObject' this type has an opinionated stance on which key is the
-- "correct" one, so we're able to have an 'At' instance.
instance Monoid ws => At (MapLikeObj ws a) where
  at k f (MLO (JObject cs)) = jAssocAlterF k f (find (textKeyMatch k) cs) <&>
    MLO . JObject . maybe (W.filter (not . textKeyMatch k) cs) (`cons` cs)

-- | Take a 'JObject' and produce a 'MapLikeObj' where the first key is
-- considered the unique value. Subsequence occurrences of that key and it's value
-- are collected and returned as a list.
toMapLikeObj :: (Semigroup ws, Monoid ws) => JObject ws a -> (MapLikeObj ws a, [JAssoc ws a])
toMapLikeObj (JObject xs) = (\(_,a,b) -> (MLO (JObject a), b)) $ foldr f (mempty,mempty,mempty) xs
  where
    f x (ys,acc,discards)
      | _jsonAssocKey x `elem` ys = (ys, acc, x:discards)
      | otherwise                 = (_jsonAssocKey x:ys, cons x acc, discards)

-- Compare a 'Text' to the key for a 'JAssoc' value.
textKeyMatch :: Text -> JAssoc ws a -> Bool
textKeyMatch k = (== Just k) . (^? jsonAssocKey . re _JString)

-- | Parse a single "key:value" pair
parseJAssoc
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JAssoc ws a)
parseJAssoc ws a = JAssoc
  <$> parseJString <*> ws <* char ':' <*> ws <*> a

-- | Builder for a single "key:value" pair.
jAssocBuilder
  :: (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JAssoc ws a
  -> Builder
jAssocBuilder ws aBuilder (JAssoc k ktws vpws v) =
  jStringBuilder k <> ws ktws <> BB.charUtf8 ':' <> ws vpws <> aBuilder ws v

-- |
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString' [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS [Space]))}, _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null, }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString' [UnescapedJChar (JCharUnescaped 'f'),UnescapedJChar (JCharUnescaped 'o'),UnescapedJChar (JCharUnescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS []))}, _elemTrailing = Just (Comma,WS [Space])}}))))
--
parseJObject
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JObject ws a)
parseJObject ws a = JObject <$>
  parseCommaSeparated (char '{') (char '}') ws (parseJAssoc ws a)

-- | Construct a 'Builder' for an entire 'JObject', duplicate keys are preserved.
jObjectBuilder
  :: (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JObject ws a
  -> Builder
jObjectBuilder ws aBuilder (JObject c) =
  commaSeparatedBuilder '{' '}' ws (jAssocBuilder ws aBuilder) c
