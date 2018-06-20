{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Types.JNumber
  ( JNumber (..)
  , HasJNumber (..)

  , E (..)
  , AsE (..)

  , Frac (..)

  , Exp (..)
  , HasExp (..)

  , JInt
  , JInt' (..)
  , _JZero
  , _JIntInt

  , jNumberBuilder
  , parseJNumber

  , jNumberToScientific
  ) where

import           Prelude                 (Bool (..), Eq, Int, Ord, Show,
                                          fromIntegral, maxBound, minBound,
                                          negate, (-), (<), (>), (||))

import           Data.Scientific         (Scientific)
import qualified Data.Scientific         as Sci

import           Control.Category        (id, (.))
import           Control.Lens            (Lens', Prism', Rewrapped,
                                          Wrapped (..), iso, prism, ( # ), (^?),
                                          _Just, _Wrapped)

import           Control.Applicative     (pure, (*>), (<$), (<$>), (<*>))
import           Control.Monad           (Monad, (=<<))

import           Data.Either             (Either (..))
import           Data.Function           (const, ($))
import           Data.Functor            (fmap)
import           Data.Maybe              (Maybe (..), fromMaybe, isJust, maybe)
import           Data.Monoid             (mappend, mempty)
import           Data.Semigroup          ((<>))
import           Data.Tuple              (uncurry)

import           Data.List.NonEmpty      (NonEmpty, some1)
import qualified Data.List.NonEmpty      as NE

import           Data.Foldable           (asum, foldMap, length)

import           Data.Digit              (Digit)
import qualified Data.Digit              as D

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many, optional, try)

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (return)
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (Digit(..))
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import qualified Data.Digit as D
-- >>> import Text.Parsec(ParseError)
-- >>> import Data.ByteString.Lazy (toStrict)
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> import Utils

data JInt' digit
  = JZero
  | JIntInt digit [Digit]
  deriving (Eq, Ord, Show)

type JInt = JInt' Digit

_JZero :: Prism' JInt ()
_JZero = prism (const JZero)
  (\case
      JZero -> Right ()
      x     -> Left x
  )

_JIntInt :: D.DecimalNoZero digit => Prism' (JInt' digit) (digit, [Digit])
_JIntInt = prism (uncurry JIntInt)
  (\case
      JIntInt d ds -> Right (d,ds)
      x -> Left x
  )

data E
  = EE
  | Ee
  deriving (Eq, Ord, Show)

class AsE r where
  _E  :: Prism' r E
  _EE :: Prism' r ()
  _Ee :: Prism' r ()
  _EE = _E . _EE
  _Ee = _E . _Ee

instance AsE E where
  _E = id
  _EE = prism (const EE)
    (\x -> case x of
        EE -> Right ()
        _  -> Left x
    )
  _Ee = prism (const Ee)
    (\x -> case x of
        Ee -> Right ()
        _  -> Left x
    )

newtype Frac = Frac (NonEmpty Digit)
  deriving (Eq, Ord, Show)

instance Frac ~ t => Rewrapped Frac t
instance Wrapped Frac where
  type Unwrapped Frac = NonEmpty Digit
  _Wrapped' = iso (\ (Frac x) -> x) Frac

data Exp = Exp
  { _ex        :: E
  , _minusplus :: Maybe Bool
  , _expdigits :: NonEmpty Digit
  }
  deriving (Eq, Ord, Show)

class HasExp c where
  exp :: Lens' c Exp
  ex :: Lens' c E
  {-# INLINE ex #-}
  expdigits :: Lens' c (NonEmpty Digit)
  {-# INLINE expdigits #-}
  minusplus :: Lens' c (Maybe Bool)
  {-# INLINE minusplus #-}
  ex = exp . ex
  expdigits = exp . expdigits
  minusplus = exp . minusplus

instance HasExp Exp where
  {-# INLINE ex #-}
  {-# INLINE expdigits #-}
  {-# INLINE minusplus #-}
  exp = id
  ex f (Exp x1 x2 x3) = fmap (\ y1 -> Exp y1 x2 x3) (f x1)
  expdigits f (Exp x1 x2 x3) = fmap (Exp x1 x2) (f x3)
  minusplus f (Exp x1 x2 x3) = fmap (\ y1 -> Exp x1 y1 x3) (f x2)

data JNumber = JNumber
  { _minus     :: Bool
  , _numberint :: JInt
  , _frac      :: Maybe Frac
  , _expn      :: Maybe Exp
  }
  deriving (Eq, Ord, Show)

class HasJNumber c where
  jNumber   :: Lens' c JNumber
  expn      :: Lens' c (Maybe Exp)
  {-# INLINE expn #-}
  frac      :: Lens' c (Maybe Frac)
  {-# INLINE frac #-}
  minus     :: Lens' c Bool
  {-# INLINE minus #-}
  numberint :: Lens' c JInt
  {-# INLINE numberint #-}
  expn      = jNumber . expn
  frac      = jNumber . frac
  minus     = jNumber . minus
  numberint = jNumber . numberint

instance HasJNumber JNumber where
  {-# INLINE expn #-}
  {-# INLINE frac #-}
  {-# INLINE minus #-}
  {-# INLINE numberint #-}
  jNumber = id
  expn f (JNumber x1 x2 x3 x4)      = fmap (JNumber x1 x2 x3) (f x4)
  frac f (JNumber x1 x2 x3 x4)      = fmap (\ y1 -> JNumber x1 x2 y1 x4) (f x3)
  minus f (JNumber x1 x2 x3 x4)     = fmap (\ y1 -> JNumber y1 x2 x3 x4) (f x1)
  numberint f (JNumber x1 x2 x3 x4) = fmap (\ y1 -> JNumber x1 y1 x3 x4) (f x2)

-- |
--
-- >>> testparse parseJInt "1"
-- Right (JIntInt 1 [])
--
-- >>> testparse parseJInt "9"
-- Right (JIntInt 9 [])
--
-- >>> testparse parseJInt "10"
-- Right (JIntInt 1 [0])
--
-- >>> testparse parseJInt "39"
-- Right (JIntInt 3 [9])
--
-- >>> testparse parseJInt "393564"
-- Right (JIntInt 3 [9,3,5,6,4])
--
-- >>> testparse parseJInt "0"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "00"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "01"
-- Right JZero
--
-- >>> testparsetheneof parseJInt "1"
-- Right (JIntInt 1 [])
--
-- >>> testparsetheneof parseJInt "9"
-- Right (JIntInt 9 [])
--
-- >>> testparsetheneof parseJInt "10"
-- Right (JIntInt 1 [0])
--
-- >>> testparsetheneof parseJInt "39"
-- Right (JIntInt 3 [9])
--
-- >>> testparsetheneof parseJInt "393564"
-- Right (JIntInt 3 [9,3,5,6,4])
--
-- >>> testparsetheneof parseJInt "0"
-- Right JZero
--
-- >>> isLeft (testparse parseJInt "x")
-- True
--
-- >>> isLeft (testparse parseJInt "")
-- True
parseJInt ::
  (Monad f, CharParsing f) =>
  f JInt
parseJInt =
  asum [
    JZero <$ try (char '0')
  , JIntInt <$> D.parseDecimalNoZero <*> many D.parseDecimal
  ]

-- |
--
-- >>> testparse parseE "e"
-- Right Ee
--
-- >>> testparse parseE "E"
-- Right EE
--
-- >>> testparsetheneof parseE "e"
-- Right Ee
--
-- >>> testparsetheneof parseE "E"
-- Right EE
--
-- >>> isLeft (testparsetheneof parseE "x")
-- True
--
-- >>> testparsethennoteof parseE "ea"
-- Right Ee
--
-- >>> testparsethennoteof parseE "Ea"
-- Right EE
parseE ::
  CharParsing f =>
  f E
parseE =
  asum [
    Ee <$ try (char 'e')
  , EE <$ char 'E'
  ]

eBuilder
  :: E
  -> Builder
eBuilder Ee = BB.charUtf8 'e'
eBuilder EE = BB.charUtf8 'E'

-- |
--
-- >>> testparsetheneof parseFrac "1"
-- Right (Frac (1 :| []))
--
-- >>> testparsetheneof parseFrac "9"
-- Right (Frac (9 :| []))
--
-- >>> testparsetheneof parseFrac "10"
-- Right (Frac (1 :| [0]))
--
-- >>> testparsetheneof parseFrac "39"
-- Right (Frac (3 :| [9]))
--
-- >>> testparsetheneof parseFrac "393564"
-- Right (Frac (3 :| [9,3,5,6,4]))
--
-- >>> testparsetheneof parseFrac "0"
-- Right (Frac (0 :| []))
--
-- >>> testparsetheneof parseFrac "00"
-- Right (Frac (0 :| [0]))
--
-- >>> testparsetheneof parseFrac "01"
-- Right (Frac (0 :| [1]))
--
-- >>> testparsethennoteof parseFrac "01x"
-- Right (Frac (0 :| [1]))
parseFrac ::
  (Monad f, CharParsing f) =>
  f Frac
parseFrac =
  Frac <$> some1 D.parseDecimal

fracBuilder
  :: Frac
  -> Builder
fracBuilder (Frac digs) =
  digitsBuilder digs

-- |
--
-- >>> testparsethen parseExp "e10x"
-- Right (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e+10x"
-- Right (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e-0x"
-- Right (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| []},'x')
--
-- >>> testparsethen parseExp "E-1x"
-- Right (Exp {_ex = EE, _minusplus = Just True, _expdigits = 1 :| []},'x')
parseExp ::
  (Monad f, CharParsing f) =>
  f Exp
parseExp = Exp
  <$> parseE
  <*> optional (asum [False <$ char '+', True <$ char '-'])
  <*> some1 D.parseDecimal

getExpSymbol
  :: Maybe Bool
  -> Builder
getExpSymbol (Just True)  = BB.charUtf8 '-'
getExpSymbol (Just False) = BB.charUtf8 '+'
getExpSymbol _            = mempty

digitsBuilder
  :: NonEmpty Digit
  -> Builder
digitsBuilder =
  foldMap (BB.int8Dec . (D.integralDecimal #))

expBuilder
  :: Exp
  -> Builder
expBuilder (Exp e sign digs) =
  eBuilder e <> getExpSymbol sign <> digitsBuilder digs

-- |
--
-- >>> testparsethen parseJNumber "600x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 6 [0,0], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "800x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 8 [0,0], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 3 [], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3x"
-- Right (JNumber {_minus = True, _numberint = JIntInt 3 [], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "0x"
-- Right (JNumber {_minus = False, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-0x"
-- Right (JNumber {_minus = True, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 3 [], _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3.45x"
-- Right (JNumber {_minus = True, _numberint = JIntInt 3 [], _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 3 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 3 [], _frac = Nothing, _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3.45e+10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt 3 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "-3.45e-02x"
-- Right (JNumber {_minus = True, _numberint = JIntInt 3 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| [2]})},'x')
--
-- >>> isLeft (testparsethen parseJNumber "-3.45ex")
-- True
--
-- >>> isLeft (testparsethen parseJNumber "-.45e1x")
-- True
parseJNumber ::
  (Monad f, CharParsing f) =>
  f JNumber
parseJNumber = JNumber
  . isJust <$> optional (char '-')
  <*> parseJInt
  <*> optional (char '.' *> parseFrac)
  <*> optional parseExp

-- | Printing of JNumbers
--
-- >>> toLazyByteString $ jNumberBuilder (JNumber {_minus = False, _numberint = JIntInt D.Digit3 [], _frac = Just (Frac (D.Digit4 :| [D.Digit5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = D.Digit1 :| [D.Digit0]})})
-- "3.45e+10"
--
-- >>> toLazyByteString $ jNumberBuilder (JNumber {_minus = True, _numberint = JIntInt D.Digit3 [], _frac = Just (Frac (D.Digit4 :| [D.Digit5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = D.Digit0 :| [D.x2]})})
-- "-3.45e-02"
--
-- >>> toLazyByteString $ jNumberBuilder (JNumber {_minus = False, _numberint = JIntInt D.Digit0 [D.Digit0], _frac = Nothing, _expn = Nothing})
-- "00"
--
jNumberBuilder
  :: JNumber
  -> Builder
jNumberBuilder (JNumber sign digs mfrac mexp) =
  s <> digits <> frac' <> expo
  where
    s      = if sign then BB.charUtf8 '-' else mempty
    digits = digitsBuilder . jIntToDigits $ digs
    frac'  = foldMap (mappend (BB.charUtf8 '.') . fracBuilder) mfrac
    expo   = foldMap expBuilder mexp

-- | Returns a normalised 'Scientific' value or Nothing if the exponent
--   is out of the range @[minBound,maxBound::Int]@
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt Digit3 [], _frac = Just (Frac (D.x4 :| [D.x5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = D.x0 :| [D.x2]})}
-- Just -3.45e-2
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt D.x1 [D.x2, D.x3], _frac = Just (Frac (D.x4 :| [D.x5, D.x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = (D.x7 :| [D.x8, D.x9])})}
-- Just -1.23456e-787
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt D.x1 [D.x2, D.x3], _frac = Just (Frac (D.x4 :| [D.x5, D.x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = (D.x7 :| [D.x8, D.x9])})}
-- Just -1.23456e791
--
jNumberToScientific :: JNumber -> Maybe Scientific
jNumberToScientific (JNumber sign int mfrac mexp) =
  if expon > fromIntegral (maxBound :: Int) ||
     expon < fromIntegral (minBound :: Int)
  then Nothing
  else Sci.scientific <$> coeff <*> pure expon
  where
    natToNeg s = fmap ( neg s . fromIntegral )

    intDigs       = jIntToDigits int

    fracList      = mfrac ^? _Just . _Wrapped
    exponentShift = maybe 0 length fracList

    coeff         = natToNeg
                      (Just sign)
                      (D.digitsToNatural $ maybe intDigs (intDigs <>) fracList)

    expon         = fromMaybe 0 ( expval =<< mexp ) - fromIntegral exponentShift

    neg (Just True) = negate
    neg _           = id

    expval (Exp _ msign digs) = natToNeg msign (D.digitsToNatural digs)

jIntToDigits :: JInt -> NonEmpty Digit
jIntToDigits JZero          = D.x0 NE.:| []
jIntToDigits (JIntInt d ds) = d NE.:| ds
