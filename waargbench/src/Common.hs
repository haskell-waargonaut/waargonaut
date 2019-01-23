{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Common
  ( parseWith
  , parseBS
  , parseText

  , testImageDataType
  , imageDecodeManual
  , imageDecodeGeneric
  , imageDecodeSuccinct
  , decodeScientific

  , Image (..)
  ) where

import qualified GHC.Generics                as GHC

import           Control.Lens                (over, _Left)
import           Control.Monad               ((>=>))
import           Data.Proxy                  (Proxy (..))

import qualified Data.List                   as List
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.ByteString             (ByteString)

import           Data.Scientific             (Scientific)

import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.Text        as AT
import           Data.Attoparsec.Types       (Parser)

import           Waargonaut                  (parseWaargonaut)
import qualified Waargonaut.Decode.Traversal as D

import qualified Waargonaut.Decode           as SD

import           Waargonaut.Decode.Error     (DecodeError (ParseFailed))
import           Waargonaut.Types            (Json)

import           Waargonaut.Generic          (GWaarg, Generic, HasDatatypeInfo,
                                              JsonDecode (..), JsonEncode (..),
                                              Options (..), defaultOpts,
                                              gDecoder, gEncoder, proxy)

data Image = Image
  { _imageWidth    :: Int
  , _imageHeight   :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
  deriving (Show, Eq, GHC.Generic)

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
imageDecodeGeneric = SD.withCursor $ SD.fromKey "Image" (proxy mkDecoder (Proxy :: Proxy GWaarg))

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

decodeScientific :: Monad f => SD.Decoder f [Scientific]
decodeScientific = proxy mkDecoder (Proxy :: Proxy GWaarg)

parseWith :: (Parser t a -> t -> Either String a) -> Parser t a -> t -> Either DecodeError a
parseWith f p = over _Left (ParseFailed . Text.pack . show) . f p

parseBS :: ByteString -> Either DecodeError Json
parseBS = parseWith AB.parseOnly parseWaargonaut

parseText :: Text -> Either DecodeError Json
parseText = parseWith AT.parseOnly parseWaargonaut
