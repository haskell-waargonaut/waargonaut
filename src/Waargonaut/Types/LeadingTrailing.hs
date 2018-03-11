{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Waargonaut.Types.LeadingTrailing where

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
