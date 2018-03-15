{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
module Waargonaut.Types.LeadingTrailing where

import           Prelude                 (Eq, Ord, Show)

import           Control.Applicative     (Applicative, (<$>), (<*>))
import           Control.Category        (id, (.))
import           Control.Lens            (Lens')

import           Data.Foldable           (Foldable)
import           Data.Functor            (Functor, fmap)
import           Data.Traversable        (Traversable)

import           Data.Char               (Char)

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Data.List               (intersperse)

import           Data.Semigroup          (mconcat, (<>))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (Digit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Text.Parsec(ParseError)
-- >>> import Data.ByteString.Lazy (toStrict)
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> import Utils

data LeadingTrailing a s = LeadingTrailing
  { _leading  :: s
  , _a        :: a
  , _trailing :: s
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class HasLeadingTrailing c a s | c -> a s where
  leadingTrailing :: Lens' c (LeadingTrailing a s)
  a :: Lens' c a
  {-# INLINE a #-}
  leading :: Lens' c s
  {-# INLINE leading #-}
  trailing :: Lens' c s
  {-# INLINE trailing #-}
  a        = leadingTrailing . a
  leading  = leadingTrailing . leading
  trailing = leadingTrailing . trailing

instance HasLeadingTrailing (LeadingTrailing a s) a s where
  {-# INLINE a #-}
  {-# INLINE leading #-}
  {-# INLINE trailing #-}
  leadingTrailing = id

  a f (LeadingTrailing x1 x2 x3) =
    fmap (\ y1 -> LeadingTrailing x1 y1 x3) (f x2)

  leading f (LeadingTrailing x1 x2 x3) =
    fmap (\ y1 -> LeadingTrailing y1 x2 x3) (f x1)

  trailing f (LeadingTrailing x1 x2 x3) =
    fmap (LeadingTrailing x1 x2) (f x3)

parseLeadingTrailing
  :: Applicative f
  => f s
  -> f a
  -> f (LeadingTrailing a s)
parseLeadingTrailing s a' =
  LeadingTrailing <$> s <*> a' <*> s

leadingTrailingBuilder
  :: (a -> Builder)
  -> (s -> Builder)
  -> LeadingTrailing a s
  -> Builder
leadingTrailingBuilder innerBuilder sBuilder lt =
  sBuilder (_leading lt) <> innerBuilder (_a lt) <> sBuilder (_trailing lt)

buildWrapped
  :: Char
  -> Char
  -> (s -> Builder)
  -> ((s -> Builder) -> a -> Builder)
  -> [LeadingTrailing a s]
  -> Builder
buildWrapped h t sB iB i =
  let
    inner = leadingTrailingBuilder (iB sB) sB <$> i
    commas = intersperse (BB.charUtf8 ',')
  in
    BB.charUtf8 h <> mconcat (commas inner) <> BB.charUtf8 t
