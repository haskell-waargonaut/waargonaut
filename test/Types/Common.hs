{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
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
  , parseText

  , testImageDataType
  , testFudge
  , imageDecodeManual
  , imageDecodeGeneric
  , imageDecodeSuccinct

  -- * Some test types to be messed with
  , Image (..)
  , Fudge (..)
  , HasImage (..)
  , Overlayed (..)
  ) where

import           Generics.SOP                (Generic, HasDatatypeInfo)
import qualified GHC.Generics                as GHC

import           Control.Lens                (makeClassy)
import           Control.Monad               ((>=>))

import           Data.Functor.Identity       (Identity)
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BSL8

import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.Text        as AT

import           Data.Tagged                 (Tagged)
import qualified Data.Tagged                 as T

import           Data.Digit                  (DecDigit, HeXDigit, HexDigit)
import qualified Data.Digit                  as D

import           Waargonaut                  (mkParseFn)
import qualified Waargonaut.Decode.Traversal as D

import qualified Waargonaut.Decode           as SD

import           Waargonaut.Decode.Error     (DecodeError)
import qualified Waargonaut.Encode           as E
import           Waargonaut.Types            (Json)
import           Waargonaut.Types.Whitespace (Whitespace (..))

import           Waargonaut.Generic          (GWaarg, JsonDecode (..),
                                              JsonEncode (..), NewtypeName (..),
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

imageDecodeSuccinct :: Monad f => SD.Decoder f Image
imageDecodeSuccinct = SD.withCursor $ SD.down >=> \curs -> do
  -- Move to the value at the "Image" key
  io <- SD.moveToKey "Image" curs >>= SD.down
  -- We need individual values off of our object,
  Image
    <$> SD.fromKey "Width" SD.int io
    <*> SD.fromKey "Height" SD.int io
    <*> SD.fromKey "Title" SD.text io
    <*> SD.fromKey "Animated" SD.bool io
    <*> SD.fromKey "IDs" (SD.list SD.int) io

imageDecodeManual :: Monad f => D.Decoder f Image
imageDecodeManual = D.withCursor $ \c -> do
  io <- D.moveToKey "Image" c

  Image
    <$> D.fromKey "Width" D.int io
    <*> D.fromKey "Height" D.int io
    <*> D.fromKey "Title" D.text io
    <*> D.fromKey "Animated" D.bool io
    <*> D.fromKey "IDs" (D.list D.int) io

imageDecodeGeneric :: Monad f => SD.Decoder f Image
imageDecodeGeneric = SD.withCursor $ SD.fromKey "Image" iDec
  -- Without using 'Proxy' type, crunchy.
  where iDec = T.untag (mkDecoder :: Monad f => Tagged GWaarg (SD.Decoder f Image))

  -- Proxy the decoder using the tag from the typeclass instance, much nicer
  -- where iDec = T.proxy mkDecoder (Proxy :: Proxy GWaarg)

  -- As above but with the niceness of TypeApplications (GHC > 8), even better
  -- where iDec = T.proxy mkDecoder (Proxy @GWaarg)

  -- Even better with using TypeApplications directly on the 'mkDecoder'
  -- where iDec = T.untag $ mkDecoder @GWaarg

instance Generic Image
instance HasDatatypeInfo Image

imageOpts :: Options
imageOpts = defaultOpts
  { _optionsFieldName = \s ->
      fromMaybe s $ List.stripPrefix "_image" s
  }

-- | You can just 'generics-sop' to automatically create an Encoder for you. Be
-- sure to check your outputs as the Generic system must make some assumptions
-- about how certain things are structured. These assumptions may not agree with
-- your expectations so always check.
instance JsonEncode GWaarg Image where mkEncoder = gEncoder imageOpts
instance JsonDecode GWaarg Image where mkDecoder = gDecoder imageOpts

newtype Fudge = Fudge Text
  deriving (Eq, Show, GHC.Generic)

instance Generic Fudge
instance HasDatatypeInfo Fudge

fudgeJsonOpts :: Options
fudgeJsonOpts = defaultOpts
  { _optionsNewtypeWithConsName = ConstructorNameAsKey
  , _optionsFieldName           = const "fudgey"
  }

instance JsonEncode GWaarg Fudge where mkEncoder = gEncoder fudgeJsonOpts
instance JsonDecode t Fudge where mkDecoder = gDecoder fudgeJsonOpts

testFudge :: Fudge
testFudge = Fudge "Chocolate"

data Overlayed = Overlayed
  { _overId :: Text
  , _overFu :: Fudge
  }
  deriving (Show, GHC.Generic)

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

parseBS :: ByteString -> Either DecodeError Json
parseBS = mkParseFn AB.parseOnly

parseText :: Text -> Either DecodeError Json
parseText = mkParseFn AT.parseOnly

prop_generic_tripping
  :: ( MonadTest m
     , Show a
     , Eq a
     )
  => Tagged GWaarg (E.Encoder Identity a)
  -> Tagged GWaarg (SD.Decoder Identity a)
  -> a
  -> m ()
prop_generic_tripping e d a = tripping a
  (E.simplePureEncodeNoSpaces (T.untag e))
  (SD.runPureDecode (T.untag d) parseBS . SD.mkCursor . BSL8.toStrict)
