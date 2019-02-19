{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Common
  ( testImageDataType
  , imageDecodeGeneric
  , imageDecode
  , decodeScientific
  , Image (..)
  ) where

import qualified GHC.Generics       as GHC

import           Control.Monad      ((>=>))
import           Data.Proxy         (Proxy (..))

import qualified Data.List          as List
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)

import           Data.Scientific    (Scientific)

import qualified Waargonaut.Decode  as D

import           Waargonaut.Generic (GWaarg, Generic, HasDatatypeInfo,
                                     JsonDecode (..), JsonEncode (..),
                                     Options (..), defaultOpts, gDecoder,
                                     gEncoder, proxy)

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

imageDecode :: Monad f =>  D.Decoder f Image
imageDecode = D.withCursor $ D.down >=> \curs -> do
  -- Move to the value at the "Image" key
  io <- D.moveToKey "Image" curs >>= D.down
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int io
    <*> D.fromKey "Height" D.int io
    <*> D.fromKey "Title" D.text io
    <*> D.fromKey "Animated" D.bool io
    <*> D.fromKey "IDs" (D.list D.int) io

imageDecodeGeneric :: Monad f =>  D.Decoder f Image
imageDecodeGeneric = D.withCursor $ D.fromKey "Image" (proxy mkDecoder (Proxy :: Proxy GWaarg))

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

decodeScientific :: Monad f =>  D.Decoder f [Scientific]
decodeScientific = proxy mkDecoder (Proxy :: Proxy GWaarg)
