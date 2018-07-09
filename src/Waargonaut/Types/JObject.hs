{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
--
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
module Waargonaut.Types.JObject
  ( JObject (..)
  , JAssoc (..)
  , emptyObj

  , HasJAssoc (..)
  , HasJObject (..)

  , MapLikeObj
  , emptyMapLike
  , toMapLikeObj
  , fromMapLikeObj

  , jObjectBuilder
  , parseJObject
  ) where

import           Prelude                   (Eq, Int, Show, elem, not, otherwise,
                                            (==))

import           Control.Applicative       ((<*), (<*>))
import           Control.Category          (id, (.))
import           Control.Lens              (At (..), Index, IxValue, Ixed (..),
                                            Lens', Rewrapped, Wrapped (..), iso, cons,
                                            ( # ), (.~), (<&>), (^?))

import           Control.Monad             (Monad)

import           Data.Bool                 (Bool (..))
import           Data.Foldable             (Foldable, find, foldr)
import           Data.Function             (($))
import           Data.Functor              (Functor, fmap, (<$>))
import           Data.Maybe                (Maybe (..), maybe)
import           Data.Semigroup            (Monoid, Semigroup, mempty, (<>))
import           Data.Text                 (Text)
import           Data.Traversable          (Traversable, traverse)

import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as BB

import qualified Data.Witherable           as W

import           Text.Parser.Char          (CharParsing, char)

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

data JAssoc ws a = JAssoc
  { _jsonAssocKey             :: JString
  , _jsonAssocKeyTrailingWS   :: ws
  , _jsonAssocValPreceedingWS :: ws
  , _jsonAssocVal             :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

newtype JObject ws a =
  JObject (CommaSeparated ws (JAssoc ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

emptyObj :: (Monoid ws, Semigroup ws) => JObject ws a
emptyObj = JObject mempty

instance JObject ws a ~ t => Rewrapped (JObject ws a) t

instance Wrapped (JObject ws a) where
  type Unwrapped (JObject ws a) = CommaSeparated ws (JAssoc ws a)
  _Wrapped' = iso (\ (JObject x) -> x) JObject

class HasJObject c ws a | c -> ws a where
  jObject :: Lens' c (JObject ws a)

instance HasJObject (JObject ws a) ws a where
  jObject = id

newtype MapLikeObj ws a = MLO
  { fromMapLikeObj :: JObject ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

emptyMapLike :: (Monoid ws, Semigroup ws) => MapLikeObj ws a
emptyMapLike = MLO emptyObj

toMapLikeObj :: (Semigroup ws, Monoid ws) => JObject ws a -> (MapLikeObj ws a, [JAssoc ws a])
toMapLikeObj (JObject xs) = (\(_,a,b) -> (MLO (JObject a), b)) $ foldr f (mempty,mempty,mempty) xs
  where
    f x (ys,acc,discards)
      | _jsonAssocKey x `elem` ys = (ys, acc, x:discards)
      | otherwise                 = (_jsonAssocKey x:ys, cons x acc, discards)

textKeyMatch :: Text -> JAssoc ws a -> Bool
textKeyMatch k = (== Just k) . (^? jsonAssocKey . _JStringText)

instance MapLikeObj ws a ~ t => Rewrapped (MapLikeObj ws a) t

instance Wrapped (MapLikeObj ws a) where
  type Unwrapped (MapLikeObj ws a) = JObject ws a
  _Wrapped' = iso (\ (MLO x) -> x) MLO

type instance IxValue (MapLikeObj ws a) = a
type instance Index (MapLikeObj ws a)   = Text

instance Monoid ws => Ixed (MapLikeObj ws a) where

jAssocAlterF :: (Monoid ws, Functor f) => Text -> (Maybe a -> f (Maybe a)) -> Maybe (JAssoc ws a) -> f (Maybe (JAssoc ws a))
jAssocAlterF k f mja = fmap g <$> f (_jsonAssocVal <$> mja) where
  g v = maybe (JAssoc (_JStringText # k) mempty mempty v) (jsonAssocVal .~ v) mja

instance Monoid ws => At (MapLikeObj ws a) where
  at k f (MLO (JObject cs)) = jAssocAlterF k f (find (textKeyMatch k) cs) <&>
    MLO . JObject . maybe (W.filter (not . textKeyMatch k) cs) (`cons` cs)

type instance IxValue (JObject ws a) = a
type instance Index (JObject ws a)   = Int

instance Monoid ws => Ixed (JObject ws a) where
  ix i f (JObject cs) = JObject <$> ix i (traverse f) cs

parseJAssoc
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JAssoc ws a)
parseJAssoc ws a = JAssoc
  <$> parseJString <*> ws <* char ':' <*> ws <*> a

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

jObjectBuilder
  :: (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> JObject ws a
  -> Builder
jObjectBuilder ws aBuilder (JObject c) =
  commaSeparatedBuilder '{' '}' ws (jAssocBuilder ws aBuilder) c
