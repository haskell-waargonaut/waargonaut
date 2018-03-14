{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Waargonaut.Types.LeadingTrailing where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Data.List               (intersperse)

import           Data.Semigroup          ((<>))

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

parseLeadingTrailing
  :: Applicative f
  => f s
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
