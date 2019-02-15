{-# LANGUAGE LambdaCase #-}
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
                                      quotRem, (&&), (||), otherwise)

import           Control.Applicative ((<*>))
import           Control.Category    (id, (.))
import           Control.Lens        (Lens', preview, review)
import           Control.Monad       ((=<<))

import Control.Error.Util (hush)

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Function (($))
import           Data.Foldable       (Foldable, foldl)
import           Data.Functor        (Functor, (<$>), fmap)
import           Data.Traversable    (Traversable, traverse)

import           Data.Char           (Char, chr, ord)
import           Data.Maybe          (Maybe (..))
import Data.Either (Either (..), either)
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

hexHeX :: D.HexDigit -> D.HeXDigit
hexHeX = \case
  D.HexDigit0 -> D.HeXDigit0
  D.HexDigit1 -> D.HeXDigit1
  D.HexDigit2 -> D.HeXDigit2
  D.HexDigit3 -> D.HeXDigit3
  D.HexDigit4 -> D.HeXDigit4
  D.HexDigit5 -> D.HeXDigit5
  D.HexDigit6 -> D.HeXDigit6
  D.HexDigit7 -> D.HeXDigit7
  D.HexDigit8 -> D.HeXDigit8
  D.HexDigit9 -> D.HeXDigit9
  D.HexDigita -> D.HeXDigita
  D.HexDigitb -> D.HeXDigitb
  D.HexDigitc -> D.HeXDigitc
  D.HexDigitd -> D.HeXDigitd
  D.HexDigite -> D.HeXDigite
  D.HexDigitf -> D.HeXDigitf

-- | Convert a given 'HexDigit4' to a Haskell 'Char'.
hexDigit4ToChar :: HexDigit4 HeXDigit -> Char
hexDigit4ToChar (HexDigit4 a b c d) = chr (D._HeXDigitsIntegral (Right $ a :| [b,c,d]))

-- | Try to convert a Haskell 'Char' to a JSON acceptable character. NOTE: This
-- cannot preserve the upper or lower casing of any original 'Json' data structure
-- inputs that may have been used to create this 'Char'. Also the JSON RFC
-- specifies a "limited" range of @U+0000@ to @U+FFFF@ as permissible as a six
-- character sequence: @\u0000@.
charToHexDigit4 :: Char -> Maybe (HexDigit4 HeXDigit)
charToHexDigit4 x
  | x < '\x0' || x > '\xffff' = Nothing
  | otherwise                 = toHexDig . fmap hexHeX =<< hush (D.integralHexDigits (ord x))
  where
    z = D.x0

    toHexDig (a :| [b,c,d]) = Just (HexDigit4 a b c d)
    toHexDig (  b :| [c,d]) = Just (HexDigit4 z b c d)
    toHexDig (    c :| [d]) = Just (HexDigit4 z z c d)
    toHexDig (     d :| []) = Just (HexDigit4 z z z d)
    toHexDig              _ = Nothing

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
