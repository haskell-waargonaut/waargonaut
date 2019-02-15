{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
-- | Types and functions for handling \\u0000 values in JSON.
module Waargonaut.Types.JChar.HexDigit4
  (
    -- * Types
    HexDigit4 (..)
  , HasHexDigit4 (..)

    -- * Parse / Build
  , parseHexDigit4
  -- , buildHexDigit4Escaped
  -- , buildHexDigit4Unescaped

    -- * Conversion
  , hexDigit4ToChar
  , charToHexDigit4
  ) where

import           Prelude             (Eq, Int, Num (..), Ord (..), Show,
                                      quotRem, (&&))

import           Control.Applicative ((<*>))
import           Control.Category    (id)
import           Control.Lens        (Lens', preview, review)
import           Control.Monad       ((=<<))

import           Data.Foldable       (Foldable, foldl)
import           Data.Functor        (Functor, (<$>))
import           Data.Traversable    (Traversable, traverse)

import           Data.Char           (Char, chr, ord)
import           Data.Maybe          (Maybe (..))

import           Text.Parser.Char    (CharParsing)

import           Data.Digit          (HeXDigit, HeXaDeCiMaL)
import qualified Data.Digit          as D

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Utils
----

-- | JSON Characters may be single escaped UTF16 "\uab34".
data HexDigit4 d =
  HexDigit4 d d d d
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

-- | Typeclass for things that contain a 'HexDigit4'.
class HasHexDigit4 c d | c -> d where
  hexDigit4 :: Lens' c (HexDigit4 d)

instance HasHexDigit4 (HexDigit4 d) d where
  hexDigit4 = id

-- | Convert a given 'HexDigit4' to a Haskell 'Char'.
hexDigit4ToChar
  :: HeXaDeCiMaL digit
  => HexDigit4 digit
  -> Char
hexDigit4ToChar (HexDigit4 a b c d) =
  chr (foldl (\acc x -> 16 * acc + (review D.integralHexadecimal x)) 0 [a,b,c,d])

charToHexDigit4
  :: Char
  -> Maybe (HexDigit4 HeXDigit)
charToHexDigit4 x = if x >= '\x0' && x <= '\xffff'
  then mkHexDigit4 =<< traverse (preview D.integralHexadecimal) (collectHexValues 4 [] (getRemainder (ord x)))
  else Nothing
  where
    mkHexDigit4 (a:b:c:d:_) = Just (HexDigit4 a b c d)
    mkHexDigit4 _           = Nothing

    getRemainder n = quotRem n 16

    collectHexValues :: Int -> [Int] -> (Int,Int) -> [Int]
    collectHexValues 0 acc _     = acc
    collectHexValues n acc (0,0) = collectHexValues (n - 1) (0:acc) (0,0)
    collectHexValues n acc (q,r) = collectHexValues (n - 1) (r:acc) (getRemainder q)
    {-# INLINE collectHexValues #-}
{-# INLINE charToHexDigit4 #-}

-- | Parse a single 'HexDigit4'.
--
-- >>> testparse parseHexDigit4 "1234" :: Either DecodeError (HexDigit4 HeXDigit)
-- Right (HexDigit4 HeXDigit1 HeXDigit2 HeXDigit3 HeXDigit4)
--
-- >>> testparse parseHexDigit4 "12aF" :: Either DecodeError (HexDigit4 HeXDigit)
-- Right (HexDigit4 HeXDigit1 HeXDigit2 HeXDigita HeXDigitF)
--
-- >>> testparse parseHexDigit4 "aBcD" :: Either DecodeError (HexDigit4 HeXDigit)
-- Right (HexDigit4 HeXDigita HeXDigitB HeXDigitc HeXDigitD)
--
-- >>> testparsetheneof parseHexDigit4 "12aF" :: Either DecodeError (HexDigit4 HeXDigit)
-- Right (HexDigit4 HeXDigit1 HeXDigit2 HeXDigita HeXDigitF)
--
-- >>> testparsethennoteof parseHexDigit4 "12aFx" :: Either DecodeError (HexDigit4 HeXDigit)
-- Right (HexDigit4 HeXDigit1 HeXDigit2 HeXDigita HeXDigitF)
parseHexDigit4 ::
  ( CharParsing f, HeXaDeCiMaL digit ) =>
  f ( HexDigit4 digit )
parseHexDigit4 = HexDigit4
  <$> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL
  <*> D.parseHeXaDeCiMaL
