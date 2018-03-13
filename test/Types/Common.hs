module Types.Common
  ( genDigit
  , genNatural
  , genText
  ) where


import           Numeric.Natural    (Natural)

import           Data.Text          (Text)

import           Hedgehog

import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import           Data.Digit         (Digit)
import qualified Data.Digit         as D

genDigit :: Gen Digit
genDigit = Gen.element
  [ D.Digit0
  , D.Digit1
  , D.Digit2
  , D.Digit3
  , D.Digit4
  , D.Digit5
  , D.Digit6
  , D.Digit7
  , D.Digit8
  , D.Digit9
  , D.Digita
  , D.Digitb
  , D.Digitc
  , D.Digitd
  , D.Digite
  , D.Digitf
  , D.DigitA
  , D.DigitB
  , D.DigitC
  , D.DigitD
  , D.DigitE
  , D.DigitF
  ]

genNatural :: Gen Natural
genNatural = fmap fromIntegral <$> Gen.filter (>= 0) $ Gen.int Range.constantBounded

genText :: Gen Text
genText = Gen.text ( Range.linear 0 100 ) Gen.unicodeAll
