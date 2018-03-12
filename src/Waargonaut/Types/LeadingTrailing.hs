{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Waargonaut.Types.LeadingTrailing where

import           Data.ByteString.Builder (Builder)

import           Data.Semigroup          ((<>))

data LeadingTrailing a s = LeadingTrailing
  { _leading  :: s
  , _a        :: a
  , _trailing :: s
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

parseLeadingTrailing ::
  Applicative f =>
  f s
  -> f a
  -> f (LeadingTrailing a s)
parseLeadingTrailing s a =
  LeadingTrailing <$> s <*> a <*> s

leadingTrailingBuilder
  :: (a -> Builder)
  -> (s -> Builder)
  -> LeadingTrailing a s
  -> Builder
leadingTrailingBuilder innerBuilder sBuilder lt =
  sBuilder (_leading lt) <> innerBuilder (_a lt) <> sBuilder (_trailing lt)
