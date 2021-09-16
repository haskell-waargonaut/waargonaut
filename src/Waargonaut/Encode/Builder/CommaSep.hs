{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
-- |
--
-- Builder functions for 'CommaSeparated' values.
--
module Waargonaut.Encode.Builder.CommaSep (commaSeparatedBuilder) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                     ((<>))
#endif

import           Waargonaut.Types.CommaSep       (Comma, CommaSeparated (..),
                                                  Elem (..), Elems (..))

import           Waargonaut.Encode.Builder.Types (Builder (..))

-- | Builder for UTF8 Comma
commaBuilder :: Builder t b -> b
commaBuilder b = fromChar b ','
{-# INLINE commaBuilder #-}

-- | Builder for a comma and trailing whitespace combination.
commaTrailingBuilder
  :: ( Monoid b
     , Foldable f
     )
  => Builder t b
  -> (Builder t b -> ws -> b)
  -> f (Comma, ws)
  -> b
commaTrailingBuilder bldr wsB =
  foldMap ((commaBuilder bldr <>) . (wsB bldr) . snd)

-- | Using the given builders for the whitespace and elements (@a@), create a
-- builder for a 'CommaSeparated'.
commaSeparatedBuilder
  :: forall ws a t b. Monoid b
  => Builder t b
  -> Char
  -> Char
  -> (Builder t b -> ws -> b)
  -> (Builder t b -> a -> b)
  -> CommaSeparated ws a
  -> b
commaSeparatedBuilder bldr op fin wsB aB (CommaSeparated lws sepElems) =
  fromChar bldr op <> wsB bldr lws <> maybe mempty buildElems sepElems <> fromChar bldr fin
  where
    elemBuilder (Elem e eTrailing) =
      aB bldr e <> commaTrailingBuilder bldr wsB eTrailing

    buildElems (Elems es elst) =
      foldMap elemBuilder es <> elemBuilder elst
