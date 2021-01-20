{-# LANGUAGE CPP #-}
-- |
--
-- Builder structures for 'Whitespace'
--
module Waargonaut.Encode.Builder.Whitespace
  ( whitespaceBuilder
  , wsBuilder
  , wsRemover
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                     (Monoid)
#endif

import           Waargonaut.Types.Whitespace     (WS (..), Whitespace (..))

import           Waargonaut.Encode.Builder.Types (Builder (..))

-- | Create a 'Data.ByteString.Builder' from a 'Whitespace'
whitespaceBuilder :: Monoid b => Builder t b -> Whitespace -> b
whitespaceBuilder bldr Space          = fromChar bldr ' '
whitespaceBuilder bldr HorizontalTab  = fromChar bldr '\t'
whitespaceBuilder bldr LineFeed       = fromChar bldr '\f'
whitespaceBuilder bldr CarriageReturn = fromChar bldr '\r'
whitespaceBuilder bldr NewLine        = fromChar bldr '\n'
{-# INLINE whitespaceBuilder #-}

-- | Reconstitute the given whitespace into its original form.
wsBuilder :: Monoid b => Builder t b -> WS -> b
wsBuilder bldr (WS ws) = foldMap (whitespaceBuilder bldr) ws
{-# INLINE wsBuilder #-}

-- | Remove any whitespace. Minification for free, yay!
wsRemover :: Monoid b => Builder t b -> WS -> b
wsRemover _ = const mempty
{-# INLINE wsRemover #-}
