{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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

  , prop_generic_tripping
  , parseBS

  , testImageDataType

  -- * Some test types to be messed with
  , Image (..)
  , HasImage (..)
  ) where

import           Generics.SOP                (Generic, HasDatatypeInfo)
import qualified GHC.Generics                as GHC

import           Control.Lens                (makeClassy)

import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BSL8

import qualified Text.Parsec                 as P

import           Data.Digit                  (DecDigit, HeXDigit, HexDigit)
import qualified Data.Digit                  as D

import           Waargonaut                  (parseWaargonaut)
import qualified Waargonaut.Decode           as D
import qualified Waargonaut.Encode           as E
import           Waargonaut.Types            (Json)
import           Waargonaut.Types.Whitespace (Whitespace (..))

import           Waargonaut.Generic          (JsonDecode (..), JsonEncode (..),
                                              Options (..), defaultOpts,
                                              gDecoder, gEncoder)

data Image = Image
  { _imageWidth    :: Int
  , _imageHeight   :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
  deriving (Show, Eq, GHC.Generic)
makeClassy ''Image

testImageDataType :: Image
testImageDataType = Image 800 600 "View from 15th Floor" False [116, 943, 234, 38793]

instance Generic Image
instance HasDatatypeInfo Image

imageOpts :: Options
imageOpts = defaultOpts
  { _optionsFieldName = \s -> fromMaybe s $ List.stripPrefix "_image" s
  }

-- | You can just 'generics-sop' to automatically create an Encoder for you,
-- however it is not a very clever system and may not behave precisely as you
-- require. Be sure to check your outputs!
instance JsonEncode Image where mkEncoder = gEncoder imageOpts
instance JsonDecode Image where mkDecoder = gDecoder imageOpts

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

parseBS :: ByteString -> Either P.ParseError Json
parseBS = P.parse parseWaargonaut "ByteString"

prop_generic_tripping
  :: ( Generic a
     , HasDatatypeInfo a
     , JsonEncode a
     , JsonDecode a
     , MonadTest m
     , Show a
     , Eq a
     )
  => a
  -> m ()
prop_generic_tripping a = tripping a
  (E.runPureEncoder mkEncoder)
  (D.simpleDecode parseBS mkDecoder . BSL8.toStrict)
