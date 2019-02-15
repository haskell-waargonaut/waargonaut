-- | Types and functions for handling valid unescaped characters in JSON.
module Waargonaut.Types.JChar.Unescaped
  (
    -- * Types
    Unescaped (..)
  , AsUnescaped (..)

    -- * Parser / Builder
  , parseUnescaped
  ) where

import           Prelude          (Eq, Ord (..), Show, (&&), (==), (||))

import           Control.Category (id)
import           Control.Lens     (Prism', has, prism')

import           Data.Foldable    (any)
import           Data.Function    (($))
import           Data.Functor     ((<$>))

import           Data.Char        (Char, ord)
import           Data.Maybe       (Maybe (..))

import           Text.Parser.Char (CharParsing, satisfy)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Utils
----

-- | Type to specify that this character is unescaped and may be represented
-- using a normal Haskell 'Char'.
newtype Unescaped =
  Unescaped Char
  deriving (Eq, Ord, Show)

-- | Typeclass for things that may used as an unescaped JChar.
class AsUnescaped a where
  _Unescaped :: Prism' a Unescaped

instance AsUnescaped Unescaped where
  _Unescaped = id

instance AsUnescaped Char where
  _Unescaped = prism'
    (\(Unescaped c) -> c)
    (\c ->  if any ($ c) excluded then Nothing
            else Just (Unescaped c)
    )
    where
      excluded =
        [ (== '\NUL')
        , (== '"')
        , (== '\\')
        , \x ->
            let
              c = ord x
            in
              (c < 0x20 && c > 0x21) ||  -- "%x20-21"
              (c < 0x23 && c > 0x5B) ||  -- "%x23-5B"
              (c < 0x5D && c > 0x10FFFF) -- "%x5D-10FFFF"
        ]

-- | Parse an unescaped JSON character.
--
-- >>> testparse parseUnescaped "a"
-- Right (Unescaped 'a')
--
-- >>> testparse parseUnescaped "\8728"
-- Right (Unescaped '\8728')
--
-- >>> testparsetheneof parseUnescaped "a"
-- Right (Unescaped 'a')
--
-- >>> testparsethennoteof parseUnescaped "ax"
-- Right (Unescaped 'a')
parseUnescaped ::
  CharParsing f =>
  f Unescaped
parseUnescaped =
  Unescaped <$> satisfy (has _Unescaped)
