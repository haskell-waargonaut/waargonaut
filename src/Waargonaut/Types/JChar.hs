{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Types and functions for handling characters in JSON.
module Waargonaut.Types.JChar
  (
    -- * Types
    HexDigit4 (..)
  , HasHexDigit4 (..)
  , JChar (..)
  , AsJChar (..)
  , HasJChar (..)
  , JCharEscaped (..)
  , AsJCharEscaped (..)
  , JCharUnescaped (..)
  , AsJCharUnescaped (..)

    -- * Parser / Builder
  , parseJChar
  , parseJCharEscaped
  , parseJCharUnescaped
  , jCharBuilderWith
  , jCharBuilderTextL
  , jCharBuilderByteStringL

  , jCharToChar

    -- * Conversion
  , utf8CharToJChar
  , jCharToUtf8Char
  ) where

import           Prelude                      (Char, Eq, Int, Ord, Show,
                                               otherwise, quotRem, (&&), (*),
                                               (+), (-), (/=), (<=), (==), (>=))

import           Control.Category             (id, (.))
import           Control.Lens                 (Lens', Prism', Rewrapped,
                                               Wrapped (..), failing, has, iso,
                                               prism, prism', to, ( # ), (^?),
                                               _Just)

import           Control.Applicative          (pure, (*>), (<$>), (<*>), (<|>))
import           Control.Monad                ((=<<))

import           Control.Error.Util           (note)

import           Data.Bits                    ((.&.))

import           Data.Char                    (chr, ord)
import           Data.Either                  (Either (..))
import           Data.Foldable                (Foldable, any, asum, foldMap,
                                               foldl)
import           Data.Function                (const, ($))
import           Data.Functor                 (Functor)
import           Data.Maybe                   (Maybe (..), fromMaybe)
import           Data.Monoid                  (Monoid)
import           Data.Semigroup               ((<>))
import           Data.Traversable             (Traversable, traverse)

import qualified Data.Text.Internal           as Text

import           Data.Digit                   (HeXDigit, HeXaDeCiMaL)
import qualified Data.Digit                   as D

import           Data.Text.Lazy.Builder       (Builder)
import qualified Data.Text.Lazy.Builder       as TB

import qualified Data.ByteString.Lazy.Builder as BB

import           Waargonaut.Types.Whitespace  (Whitespace (..),
                                               escapedWhitespaceChar,
                                               unescapedWhitespaceChar,
                                               _WhitespaceChar)

import           Text.Parser.Char             (CharParsing, char, satisfy)

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

-- | Type to specify that this character is unescaped and may be represented
-- using a normal Haskell 'Char'.
newtype JCharUnescaped =
  JCharUnescaped Char
  deriving (Eq, Ord, Show)

instance JCharUnescaped ~ t => Rewrapped JCharUnescaped t
instance Wrapped JCharUnescaped where
  type Unwrapped JCharUnescaped = Char
  _Wrapped' = iso (\ (JCharUnescaped x) -> x) JCharUnescaped

-- | Typeclass for things that may used as an unescaped JChar.
class AsJCharUnescaped a where
  _JCharUnescaped :: Prism' a JCharUnescaped

instance AsJCharUnescaped JCharUnescaped where
  _JCharUnescaped = id

instance AsJCharUnescaped Char where
  _JCharUnescaped = prism'
    (\(JCharUnescaped c) -> c)
    (\c ->  if any ($ c) excluded then Nothing
            else Just (JCharUnescaped c)
    )
    where
      excluded =
        [ (== '\NUL')
        , (== '\DEL')
        , (== '"')
        , (== '\\')
        , \x -> x >= '\x00' && x <= '\x1f'
        ]

-- | Things that may be escaped in a JSON string.
data JCharEscaped digit
  = QuotationMark
  | ReverseSolidus
  | Solidus
  | Backspace
  | WhiteSpace Whitespace
  | Hex ( HexDigit4 digit )
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Typeclass for things that may be used as an escaped JChar.
class AsJCharEscaped r digit | r -> digit where
  _JCharEscaped   :: Prism' r (JCharEscaped digit)
  _QuotationMark  :: Prism' r ()
  _ReverseSolidus :: Prism' r ()
  _Solidus        :: Prism' r ()
  _Backspace      :: Prism' r ()
  _WhiteSpace     :: Prism' r Whitespace
  _Hex            :: Prism' r (HexDigit4 digit)

  _QuotationMark  = _JCharEscaped . _QuotationMark
  _ReverseSolidus = _JCharEscaped . _ReverseSolidus
  _Solidus        = _JCharEscaped . _Solidus
  _Backspace      = _JCharEscaped . _Backspace
  _WhiteSpace     = _JCharEscaped . _WhiteSpace
  _Hex            = _JCharEscaped . _Hex

instance AsJCharEscaped (JCharEscaped digit) digit where
  _JCharEscaped = id
  _QuotationMark = prism (const QuotationMark)
    (\ x -> case x of
        QuotationMark -> Right ()
        _             -> Left x
    )
  _ReverseSolidus = prism (const ReverseSolidus)
    (\ x -> case x of
        ReverseSolidus -> Right ()
        _              -> Left x
    )
  _Solidus = prism (const Solidus)
    (\ x -> case x of
        Solidus -> Right ()
        _       -> Left x
    )
  _Backspace = prism (const Backspace)
    (\ x -> case x of
        Backspace -> Right ()
        _         -> Left x
    )
  _WhiteSpace = prism WhiteSpace
    (\ x -> case x of
        WhiteSpace y1 -> Right y1
        _             -> Left x
    )
  _Hex = prism Hex
    (\ x -> case x of
        Hex y1 -> Right y1
        _      -> Left x
    )

-- | Convert a given 'HexDigit4' to a Haskell 'Char'.
hexDigit4ToChar
  :: HeXaDeCiMaL digit
  => HexDigit4 digit
  -> Char
hexDigit4ToChar (HexDigit4 a b c d) =
  chr (foldl (\acc x -> 16 * acc + (D.integralHexadecimal # x)) 0 [a,b,c,d])

sandblast :: Char -> Maybe (HexDigit4 HeXDigit)
sandblast x = if x >= '\x0' && x <= '\xffff'
  then shuriken =<< traverse (^? D.integralHexadecimal) (lavawave 4 [] (bile (ord x)))
  else Nothing
  where
    shuriken (a:b:c:d:_) = Just (HexDigit4 a b c d)
    shuriken _           = Nothing

    bile n = quotRem n 16

    lavawave :: Int -> [Int] -> (Int,Int) -> [Int]
    lavawave 0 acc _     = acc
    lavawave n acc (0,0) = lavawave (n - 1) (0:acc) (0,0)
    lavawave n acc (q,r) = lavawave (n - 1) (r:acc) (bile q)
    {-# INLINE lavawave #-}
{-# INLINE sandblast #-}

instance AsJCharEscaped Char HeXDigit where
  _JCharEscaped = prism
    (\case
        QuotationMark  -> '"'
        ReverseSolidus -> '\\'
        Solidus        -> '/'
        Backspace      -> '\b'
        WhiteSpace wc  -> escapedWhitespaceChar wc
        Hex hd         -> hexDigit4ToChar hd
    )
    (\c -> case c of
        '"'  -> Right QuotationMark
        '\\' -> Right ReverseSolidus
        '/'  -> Right Solidus
        '\b' -> Right Backspace
        _    -> note c $ c ^? failing (_WhitespaceChar . to WhiteSpace) (to sandblast . _Just . to Hex)
    )
-- | A JChar may be unescaped or escaped.
data JChar digit
  = EscapedJChar ( JCharEscaped digit )
  | UnescapedJChar JCharUnescaped
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Typeclass for things that have a 'JChar'.
class HasJChar c digit | c -> digit where
  jChar :: Lens' c (JChar digit)

instance HasJChar (JChar digit) digit where
  jChar = id

-- | Typeclass for things that be used as a 'JChar'.
class AsJChar r digit | r -> digit where
  _JChar          :: Prism' r (JChar digit)
  _EscapedJChar   :: Prism' r (JCharEscaped digit)
  _UnescapedJChar :: Prism' r JCharUnescaped

  _EscapedJChar   = _JChar . _EscapedJChar
  _UnescapedJChar = _JChar . _UnescapedJChar

instance AsJChar (JChar digit) digit where
  _JChar = id
  _EscapedJChar = prism EscapedJChar
    (\ x -> case x of
        EscapedJChar y1 -> Right y1
        _               -> Left x
    )
  _UnescapedJChar = prism UnescapedJChar
    (\ x -> case x of
        UnescapedJChar y1 -> Right y1
        _                 -> Left x
    )

instance AsJCharEscaped (JChar digit) digit where
  _JCharEscaped = _JChar . _JCharEscaped

instance AsJCharUnescaped (JChar digit) where
  _JCharUnescaped = _JChar . _JCharUnescaped

instance AsJChar Char HeXDigit where
  _JChar = prism
    (\case
        UnescapedJChar jcu -> _JCharUnescaped # jcu
        EscapedJChar jce   -> _JCharEscaped # jce
    )
    (\c -> note c $ c ^? failing
      (_JCharUnescaped . to UnescapedJChar)
      (_JCharEscaped . to EscapedJChar)
    )

-- | Helper to determine if the given 'Char' is an acceptable utf8 value.
utf8SafeChar :: Char -> Maybe Char
utf8SafeChar c | ord c .&. 0x1ff800 /= 0xd800 = Just c
               | otherwise                    = Nothing

-- | Convert a 'Char' to 'JChar HexDigit' and replace any invalid values with
-- @U+FFFD@ as per the 'Text' documentation.
--
-- Refer to <https://hackage.haskell.org/package/text/docs/Data-Text.html#g:2 'Text'> documentation for more info.
--
utf8CharToJChar :: Char -> JChar HeXDigit
utf8CharToJChar c = fromMaybe scalarReplacement (Text.safe c ^? _JChar)
  where scalarReplacement = EscapedJChar (Hex (HexDigit4 D.xf D.xf D.xf D.xd))
{-# INLINE utf8CharToJChar #-}

-- | Try to convert a 'JChar' to a Haskell 'Char'.
jCharToUtf8Char :: JChar HeXDigit -> Maybe Char
jCharToUtf8Char jc = utf8SafeChar (_JChar # jc)
{-# INLINE jCharToUtf8Char #-}

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

-- | Parse an unescaped JSON character.
--
-- >>> testparse parseJCharUnescaped "a"
-- Right (JCharUnescaped 'a')
--
-- >>> testparse parseJCharUnescaped "\8728"
-- Right (JCharUnescaped '\8728')
--
-- >>> testparsetheneof parseJCharUnescaped "a"
-- Right (JCharUnescaped 'a')
--
-- >>> testparsethennoteof parseJCharUnescaped "ax"
-- Right (JCharUnescaped 'a')
parseJCharUnescaped ::
  CharParsing f =>
  f JCharUnescaped
parseJCharUnescaped =
  JCharUnescaped <$> satisfy (has _JCharUnescaped)

-- | Parse an escapted JSON character.
--
-- >>> testparse parseJCharEscaped "\\\""
-- Right QuotationMark
--
-- >>> testparse parseJCharEscaped "\\\\"
-- Right ReverseSolidus
--
-- >>> testparse parseJCharEscaped "\\/"
-- Right Solidus
--
-- >>> testparse parseJCharEscaped "\\b"
-- Right Backspace
--
-- >>> testparse parseJCharEscaped "\\f"
-- Right (WhiteSpace LineFeed)
--
-- >>> testparse parseJCharEscaped "\\n"
-- Right (WhiteSpace NewLine)
--
-- >>> testparse parseJCharEscaped "\\r"
-- Right (WhiteSpace CarriageReturn)
--
-- >>> testparse parseJCharEscaped "\\t"
-- Right (WhiteSpace HorizontalTab)
--
-- >>> testparse parseJCharEscaped "\\u1234" :: Either DecodeError (JCharEscaped HeXDigit)
-- Right (Hex (HexDigit4 HeXDigit1 HeXDigit2 HeXDigit3 HeXDigit4))
--
-- >>> testparsetheneof parseJCharEscaped "\\t"
-- Right (WhiteSpace HorizontalTab)
--
-- >>> testparsethennoteof parseJCharEscaped "\\tx"
-- Right (WhiteSpace HorizontalTab)
parseJCharEscaped ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JCharEscaped digit )
parseJCharEscaped =
  let
    z =
      asum ((\(c, p) -> char c *> pure p) <$>
        [
          ('"' , QuotationMark)
        , ('\\', ReverseSolidus)
        , ('/' , Solidus)
        , ('b' , Backspace)
        , (' ' , WhiteSpace Space)
        , ('f' , WhiteSpace LineFeed)
        , ('n' , WhiteSpace NewLine)
        , ('r' , WhiteSpace CarriageReturn)
        , ('t' , WhiteSpace HorizontalTab)
        ])
    h =
      Hex <$> (char 'u' *> parseHexDigit4)
  in
    char '\\' *> (z <|> h)

-- | Parse a JSON character.
--
-- >>> testparse parseJChar "\\u1234" :: Either DecodeError (JChar HeXDigit)
-- Right (EscapedJChar (Hex (HexDigit4 HeXDigit1 HeXDigit2 HeXDigit3 HeXDigit4)))
--
-- >>> testparse parseJChar "\\\\" :: Either DecodeError (JChar HeXDigit)
-- Right (EscapedJChar ReverseSolidus)
--
-- >>> testparse parseJChar "\\r"
-- Right (EscapedJChar (WhiteSpace CarriageReturn))
--
-- >>> testparsetheneof parseJChar "a"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
--
-- >>> testparsethennoteof parseJChar "ax"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
parseJChar ::
  (CharParsing f, HeXaDeCiMaL digit) =>
  f ( JChar digit )
parseJChar = asum
  [ EscapedJChar <$> parseJCharEscaped
  , UnescapedJChar <$> parseJCharUnescaped
  ]

-- | Convert a 'JChar' to a Haskell 'Char'.
jCharToChar
  :: HeXaDeCiMaL digit
  => JChar digit
  -> Char
jCharToChar (UnescapedJChar (JCharUnescaped c)) = c
jCharToChar (EscapedJChar jca) = case jca of
    QuotationMark   -> '"'
    ReverseSolidus  -> '\\'
    Solidus         -> '/'
    Backspace       -> '\b'
    (WhiteSpace ws) -> _WhiteSpace # ws
    Hex hexDig4     -> hexDigit4ToChar hexDig4

jCharBuilderWith
  :: ( Monoid builder
     , HeXaDeCiMaL digit
     )
  => (Char -> builder)
  -> JChar digit
  -> builder
jCharBuilderWith f (UnescapedJChar (JCharUnescaped c)) = f c
jCharBuilderWith f (EscapedJChar jca) = f '\\' <> case jca of
    QuotationMark           -> f '"'
    ReverseSolidus          -> f '\\'
    Solidus                 -> f '/'
    Backspace               -> f 'b'
    (WhiteSpace ws)         -> f (unescapedWhitespaceChar ws)
    Hex (HexDigit4 a b c d) -> f 'u' <> foldMap hexChar [a,b,c,d]
  where
    hexChar =
      f . (D.charHeXaDeCiMaL #)

-- | Create a Lazy 'Text' 'Data.Text.Lazy.Builder' for the given 'JChar'.
jCharBuilderTextL :: HeXaDeCiMaL digit => JChar digit -> Builder
jCharBuilderTextL = jCharBuilderWith TB.singleton

-- | Create a Lazy 'ByteString' 'Data.ByteString.Lazy.Builder' for a given 'JChar'
jCharBuilderByteStringL :: HeXaDeCiMaL digit => JChar digit -> BB.Builder
jCharBuilderByteStringL = jCharBuilderWith BB.charUtf8
