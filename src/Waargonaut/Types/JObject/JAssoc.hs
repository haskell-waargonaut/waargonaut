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
-- | Types and functions for handling our representation of a key:value pair in
-- a JSON object.
module Waargonaut.Types.JObject.JAssoc
  (
    -- * Key/value pair type
    JAssoc (..)
  , HasJAssoc (..)

    -- * Parse / Build
  , parseJAssoc
  , jAssocBuilder

    -- * Update
  , jAssocAlterF
  ) where

import           Prelude                  (Eq, Show)

import           Control.Applicative      ((<*), (<*>))
import           Control.Category         (id, (.))
import           Control.Lens             (Lens', ( # ), (.~))

import           Control.Monad            (Monad)
import           Data.Bifoldable          (Bifoldable (bifoldMap))
import           Data.Bifunctor           (Bifunctor (bimap))
import           Data.Bitraversable       (Bitraversable (bitraverse))
import           Data.Foldable            (Foldable)
import           Data.Functor             (Functor, fmap, (<$>))
import           Data.Maybe               (Maybe (..), maybe)
import           Data.Monoid              (Monoid (mappend, mempty))
import           Data.Semigroup           (Semigroup ((<>)))
import           Data.Text                (Text)
import           Data.Traversable         (Traversable)

import           Data.Text.Lazy.Builder   (Builder)
import qualified Data.Text.Lazy.Builder   as TB

import           Text.Parser.Char         (CharParsing, char)

import           Waargonaut.Types.JString (JString, jStringBuilder,
                                           parseJString, _JStringText)

-- | This type represents the key:value pair inside of a JSON object.
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

instance Bifunctor JAssoc where
  bimap f g (JAssoc k w1 w2 v) = JAssoc k (f w1) (f w2) (g v)

instance Bifoldable JAssoc where
  bifoldMap f g (JAssoc _ w1 w2 v) = f w1 `mappend` f w2 `mappend` g v

instance Bitraversable JAssoc where
  bitraverse f g (JAssoc k w1 w2 v) = JAssoc k <$> f w1 <*> f w2 <*> g v

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

-- | Helper function for trying to update/create a JAssoc value in some Functor.
-- This function is analogus to the 'Data.Map.alterF' function.
jAssocAlterF
  :: ( Monoid ws
     , Functor f
     )
  => Text
  -> (Maybe a -> f (Maybe a))
  -> Maybe (JAssoc ws a)
  -> f (Maybe (JAssoc ws a))
jAssocAlterF k f mja = fmap g <$> f (_jsonAssocVal <$> mja) where
  g v = maybe (JAssoc (_JStringText # k) mempty mempty v) (jsonAssocVal .~ v) mja

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
  jStringBuilder k <> ws ktws <> TB.singleton ':' <> ws vpws <> aBuilder ws v
