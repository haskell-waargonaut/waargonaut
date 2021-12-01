{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE CPP                    #-}
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
  , _MapLikeObj

    -- * Parser
  , parseJObject
  ) where

import           Prelude                         (Eq, Int, Show, elem, fst, not,
                                                  otherwise, (==))

import           Control.Category                (id, (.))
import           Control.Lens                    (AsEmpty (..), At (..), Index,
                                                  IxValue, Ixed (..), Lens',
                                                  Prism', Rewrapped,
                                                  Wrapped (..), cons, iso,
                                                  nearly, prism', to, ( # ),
                                                  (<&>), (^.), _Wrapped)
import           Control.Lens.Extras             (is)

import           Control.Monad                   (Monad)
import           Data.Bifoldable                 (Bifoldable (bifoldMap))
import           Data.Bifunctor                  (Bifunctor (bimap))
import           Data.Bitraversable              (Bitraversable (bitraverse))
import           Data.Bool                       (Bool (..))
import           Data.Foldable                   (Foldable, find, foldr)
import           Data.Function                   (($))
import           Data.Functor                    (Functor, (<$>))
import           Data.Maybe                      (Maybe (..), maybe)
import           Data.Monoid                     (Monoid (mappend, mempty))
import           Data.Semigroup                  (Semigroup ((<>)))
import           Data.Text                       (Text)
import           Data.Traversable                (Traversable, traverse)

#if MIN_VERSION_witherable(0,4,0)
import qualified Witherable                      as W
#else
import qualified Data.Witherable                 as W
#endif

import           Text.Parser.Char                (CharParsing, char)

import           Waargonaut.Types.CommaSep       (CommaSeparated (..),
                                                  parseCommaSeparated)

import           Waargonaut.Types.JObject.JAssoc (HasJAssoc (..), JAssoc (..),
                                                  jAssocAlterF, parseJAssoc)

import           Waargonaut.Types.JString        (_JStringText)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Data.Digit (HeXDigit)
----

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

instance (Semigroup ws, Monoid ws) => AsEmpty (JObject ws a) where
  _Empty = nearly (_Wrapped # _Empty # ()) (^. _Wrapped . to (is _Empty))
  {-# INLINE _Empty #-}

instance JObject ws a ~ t => Rewrapped (JObject ws a) t

instance Wrapped (JObject ws a) where
  type Unwrapped (JObject ws a) = CommaSeparated ws (JAssoc ws a)
  _Wrapped' = iso (\ (JObject x) -> x) JObject

type instance IxValue (JObject ws a) = a
type instance Index (JObject ws a)   = Int

instance (Semigroup ws, Monoid ws) => Semigroup (JObject ws a) where
  (JObject a) <> (JObject b) = JObject (a <> b)

instance (Semigroup ws, Monoid ws) => Monoid (JObject ws a) where
  mempty = JObject mempty
  mappend = (<>)

instance Bifunctor JObject where
  bimap f g (JObject c) = JObject (bimap f (bimap f g) c)

instance Bifoldable JObject where
  bifoldMap f g (JObject c) = bifoldMap f (bifoldMap f g) c

instance Bitraversable JObject where
  bitraverse f g (JObject c) = JObject <$> bitraverse f (bitraverse f g) c

-- | Without having an obviously correct "first" or "last" decision on which
-- 'Waargonaut.Types.JString' key is the "right" one to use, a 'JObject' can only be indexed by a
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

-- |
-- 'Control.Lens.Prism' for working with a 'JObject' as a 'MapLikeObj'. This optic will keep
-- the first unique key on a given 'JObject' and this information is not
-- recoverable. If you want to create a 'MapLikeObj' from a 'JObject' and keep
-- what is removed, then use the 'toMapLikeObj' function.
--
_MapLikeObj :: (Semigroup ws, Monoid ws) => Prism' (JObject ws a) (MapLikeObj ws a)
_MapLikeObj = prism' fromMapLikeObj (Just . fst . toMapLikeObj)

instance MapLikeObj ws a ~ t => Rewrapped (MapLikeObj ws a) t

instance Wrapped (MapLikeObj ws a) where
  type Unwrapped (MapLikeObj ws a) = JObject ws a
  _Wrapped' = iso (\ (MLO x) -> x) MLO

instance (Monoid ws, Semigroup ws) => AsEmpty (MapLikeObj ws a) where
  _Empty = nearly (_Wrapped # _Empty # ()) (^. _Wrapped . to (is _Empty))
  {-# INLINE _Empty #-}

type instance IxValue (MapLikeObj ws a) = a
type instance Index (MapLikeObj ws a)   = Text

instance Monoid ws => Ixed (MapLikeObj ws a) where

-- | Unlike 'JObject' this type has an opinionated stance on which key is the
-- "correct" one, so we're able to have an 'At' instance.
instance Monoid ws => At (MapLikeObj ws a) where
  at k f (MLO (JObject cs)) = jAssocAlterF k f (find (textKeyMatch k) cs) <&>
    MLO . JObject . maybe (W.filter (not . textKeyMatch k) cs) (`cons` cs)

instance Bifunctor MapLikeObj where
  bimap f g (MLO o) = MLO (bimap f g o)

instance Bifoldable MapLikeObj where
  bifoldMap f g (MLO o) = bifoldMap f g o

instance Bitraversable MapLikeObj where
  bitraverse f g (MLO o) = MLO <$> bitraverse f g o

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
textKeyMatch k = (== k) . (^. jsonAssocKey . _JStringText)

-- |
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString' [UnescapedJChar (Unescaped 'f'),UnescapedJChar (Unescaped 'o'),UnescapedJChar (Unescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS [Space]))}, _elemTrailing = Nothing}}))))
--
-- >>> testparse (parseJObject parseWhitespace parseWaargonaut) "{\"foo\":null, }"
-- Right (JObject (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = JAssoc {_jsonAssocKey = JString' [UnescapedJChar (Unescaped 'f'),UnescapedJChar (Unescaped 'o'),UnescapedJChar (Unescaped 'o')], _jsonAssocKeyTrailingWS = WS [], _jsonAssocValPreceedingWS = WS [], _jsonAssocVal = Json (JNull (WS []))}, _elemTrailing = Just (Comma,WS [Space])}}))))
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
