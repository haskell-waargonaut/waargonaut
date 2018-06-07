module Types.JNumber
  ( genJNumber
  ) where

import           Control.Applicative      (liftA2)
import           Control.Lens             (( # ))

import           Hedgehog
import qualified Hedgehog.Gen             as Gen

import           Types.Common             (genDecimalDigitNoZero,
                                           genDecimalDigits,
                                           genNonEmptyDecimalDigit)

import           Waargonaut.Types.JNumber (E (..), Exp (..), Frac (..), JInt,
                                           JNumber (..), _JIntInt, _JZero)

genJNumber :: Gen JNumber
genJNumber = JNumber
  <$> Gen.bool
  <*> genJInt
  <*> Gen.maybe genFrac
  <*> Gen.maybe genExp

genJInt :: Gen JInt
genJInt = Gen.choice
  [ Gen.constant (_JZero # ())
  , (_JIntInt #) <$> liftA2 (,) genDecimalDigitNoZero genDecimalDigits
  ]

genFrac :: Gen Frac
genFrac = Frac <$> genNonEmptyDecimalDigit

genExp :: Gen Exp
genExp = Exp
  <$> Gen.element [EE,Ee]
  <*> Gen.maybe Gen.bool
  <*> genNonEmptyDecimalDigit
