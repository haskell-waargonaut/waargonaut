{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Types.Common
  ( genDecimalDigit
  , genDecimalDigits
  , genDecimalDigitNoZero
  , genHeXaDeCiMaLDigit
  , genHeXaDeCiMaLDigitNoZero
  , genNonEmptyDecimalDigit
  , genText
  , genWhitespace

  , prop_generic_tripping
  , parseBS
  , parseText
  , encodeJsonText
  , encodeText
  , encodeBS
  , decodeText
  , simpleDecodeWith

  , testImageDataType
  , testFudge
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
import qualified Data.Text.Lazy              as TextL

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Data.ByteString             (ByteString)

import qualified Data.ByteString.Lazy        as BL

import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.Text        as AT

import           Data.Tagged                 (Tagged)
import qualified Data.Tagged                 as T

import           Data.Digit                  (DecDigit, HeXDigit)
import qualified Data.Digit                  as D

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

parseBS :: SD.Decoder Identity a -> ByteString -> Either (DecodeError, SD.CursorHistory) a
parseBS d = SD.pureDecodeFromByteString AB.parseOnly d

parseText :: SD.Decoder Identity a -> Text -> Either (DecodeError, SD.CursorHistory) a
parseText d = SD.pureDecodeFromText AT.parseOnly d

encodeJsonText :: Json -> Text
encodeJsonText = TextL.toStrict . E.simplePureEncodeText E.json

encodeText :: E.Encoder Identity a -> a -> TextL.Text
encodeText e = E.simplePureEncodeText e

encodeBS :: Json -> ByteString
encodeBS = BL.toStrict . E.simplePureEncodeByteString E.json

decodeText :: Text -> Either (DecodeError, SD.CursorHistory) Json
decodeText = SD.pureDecodeFromText AT.parseOnly SD.json

simpleDecodeWith :: SD.Decoder Identity a -> TextL.Text -> Either (DecodeError, SD.CursorHistory) a
simpleDecodeWith d = SD.pureDecodeFromText AT.parseOnly d . TextL.toStrict

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
  (E.simplePureEncodeTextNoSpaces (T.untag e))
  (simpleDecodeWith (T.untag d))
