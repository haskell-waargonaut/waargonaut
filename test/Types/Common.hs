{-# LANGUAGE TemplateHaskell #-}
module Types.Common
  ( genDecimalDigit
  , genDecimalDigits
  , genDecimalDigitNoZero
  , genHeXaDeCiMaLDigit
  , genHeXaDeCiMaLDigitNoZero
  , genHexadecimalDigitLower
  , genNonEmptyDecimalDigit
  , genText
  , genWhitespace
  , hexadecimalDigitLower

  -- * Some test types to be messed with
  , Image (..)
  , HasImage (..)
  ) where

import Control.Lens (makeClassy)

import           Data.List.NonEmpty          (NonEmpty)
import           Data.Text                   (Text)

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Data.Digit                  (DecDigit, HeXDigit, HexDigit)
import qualified Data.Digit                  as D

import           Waargonaut.Types.Whitespace (Whitespace (..))


data Image = Image
  { _imageW        :: Int
  , _imageH        :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
  deriving Show
makeClassy ''Image

genDecimalDigit :: Gen DecDigit
genDecimalDigit = Gen.element decimalDigit

genHexadecimalDigitLower :: Gen HexDigit
genHexadecimalDigitLower = Gen.element hexadecimalDigitLower

genHeXaDeCiMaLDigit :: Gen HeXDigit
genHeXaDeCiMaLDigit = Gen.element hExAdEcImAlDigit

decimalDigit :: [DecDigit]
decimalDigit =
  [ D.DecDigit0
  , D.DecDigit1
  , D.DecDigit2
  , D.DecDigit3
  , D.DecDigit4
  , D.DecDigit5
  , D.DecDigit6
  , D.DecDigit7
  , D.DecDigit8
  , D.DecDigit9
  ]

hexadecimalDigitLower :: [HexDigit]
hexadecimalDigitLower =
  [ D.HexDigita
  , D.HexDigitb
  , D.HexDigitc
  , D.HexDigitd
  , D.HexDigite
  , D.HexDigitf
  ]

hExAdEcImAlDigit :: [HeXDigit]
hExAdEcImAlDigit =
  [ D.HeXDigit0
  , D.HeXDigit1
  , D.HeXDigit2
  , D.HeXDigit3
  , D.HeXDigit4
  , D.HeXDigit5
  , D.HeXDigit6
  , D.HeXDigit7
  , D.HeXDigit8
  , D.HeXDigit9
  , D.HeXDigita
  , D.HeXDigitb
  , D.HeXDigitc
  , D.HeXDigitd
  , D.HeXDigite
  , D.HeXDigitf
  , D.HeXDigitA
  , D.HeXDigitB
  , D.HeXDigitC
  , D.HeXDigitD
  , D.HeXDigitE
  , D.HeXDigitF
  ]

genDecimalDigitNoZero :: Gen DecDigit
genDecimalDigitNoZero = Gen.filter (/= D.DecDigit0) genDecimalDigit

genHeXaDeCiMaLDigitNoZero :: Gen HeXDigit
genHeXaDeCiMaLDigitNoZero = Gen.filter (/= D.HeXDigit0) genHeXaDeCiMaLDigit

genDecimalDigits :: Gen [DecDigit]
genDecimalDigits = Gen.list (Range.linear 1 10) genDecimalDigit

genNonEmptyDecimalDigit :: Gen (NonEmpty DecDigit)
genNonEmptyDecimalDigit = Gen.nonEmpty (Range.linear 1 10) genDecimalDigit

genWhitespace :: Gen Whitespace
genWhitespace = Gen.element
  [ Space
  , HorizontalTab
  , LineFeed
  , NewLine
  , CarriageReturn
  ]

genText :: Gen Text
genText = Gen.text ( Range.linear 0 100 ) Gen.unicodeAll
