-- | 
--
-- Builders for 'JNumber'
--
module Waargonaut.Encode.Builder.JNumber
  ( jNumberBuilder
  ) where

import           Control.Lens                    (review)

import qualified Data.Digit                      as D
import           Data.List.NonEmpty              (NonEmpty)
import           Data.Monoid                     ((<>))

import           Waargonaut.Types.JNumber        (E (..), Exp (..), Frac (..),
                                                  JNumber (..), jIntToDigits)

import           Waargonaut.Encode.Builder.Types (Builder (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Digit (DecDigit(..))
-- >>> import qualified Data.Digit as D
-- >>> import Data.Text.Lazy.Builder (toLazyText)
-- >>> import Waargonaut.Encode.Builder (textBuilder)
-- >>> import Waargonaut.Types.JNumber (JInt' (JIntInt))
--

getExpSymbol
  :: Monoid b
  => Builder t b
  -> Maybe Bool
  -> b
getExpSymbol bldr (Just True)  = fromChar bldr '-'
getExpSymbol bldr (Just False) = fromChar bldr '+'
getExpSymbol _    _            = mempty

eBuilder
  :: Monoid b
  => Builder t b
  -> E
  -> b
eBuilder bldr Ee = fromChar bldr 'e'
eBuilder bldr EE = fromChar bldr 'E'

fracBuilder :: Monoid b => Builder t b -> Frac -> b
fracBuilder bldr (Frac digs) = digitsBuilder bldr digs

digitsBuilder
  :: Monoid b
  => Builder t b
  -> NonEmpty D.DecDigit
  -> b
digitsBuilder bldr =
  foldMap (fromInt bldr . review D.integralDecimal)

-- | Builder for the exponent portion.
expBuilder
  :: Monoid b
  => Builder t b
  -> Exp
  -> b
expBuilder bldr (Exp e sign digs) =
  eBuilder bldr e <> getExpSymbol bldr sign <> digitsBuilder bldr digs

-- | Printing of JNumbers
--
-- >>> toLazyText $ jNumberBuilder textBuilder (JNumber {_minus = False, _numberint = JIntInt D.DecDigit3 [], _frac = Just (Frac (D.DecDigit4 :| [D.DecDigit5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = D.DecDigit1 :| [D.DecDigit0]})})
-- "3.45e+10"
--
-- >>> toLazyText $ jNumberBuilder textBuilder (JNumber {_minus = True, _numberint = JIntInt D.DecDigit3 [], _frac = Just (Frac (D.DecDigit4 :| [D.DecDigit5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = D.DecDigit0 :| [D.x2]})})
-- "-3.45e-02"
--
-- >>> toLazyText $ jNumberBuilder textBuilder (JNumber {_minus = False, _numberint = JIntInt D.DecDigit0 [D.DecDigit0], _frac = Nothing, _expn = Nothing})
-- "00"
--
jNumberBuilder
  :: Monoid b
  => Builder t b
  -> JNumber
  -> b
jNumberBuilder bldr (JNumber sign digs mfrac mexp) =
  s <> digits <> frac' <> expo
  where
    s      = if sign then fromChar bldr '-' else mempty
    digits = digitsBuilder bldr . jIntToDigits $ digs
    frac'  = foldMap (mappend (fromChar bldr '.') . fracBuilder bldr) mfrac
    expo   = foldMap (expBuilder bldr) mexp
