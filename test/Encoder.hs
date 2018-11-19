{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Encoder
  ( encoderTests
  , encodeImage
  , testImageDataType
  ) where

import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (assertEqual, testCase)

import           Data.Proxy           (Proxy (..))

import           Waargonaut.Encode    (Encoder, Encoder')
import qualified Waargonaut.Encode    as E

import           Data.ByteString.Lazy (ByteString)

import           Types.Common         (Image (..), testFudge, testImageDataType)

import           Waargonaut.Generic   (GWaarg, mkEncoder, proxy)

testImageEncodedNoSpaces :: ByteString
testImageEncodedNoSpaces = "{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"

-- | The recommended way of defining an Encoder is to be explicit.
encodeImage :: Applicative f => Encoder f Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageWidth img)
  . E.intAt "Height" (_imageHeight img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.listAt E.int "IDs" (_imageIDs img)

testFudgeEncodedWithConsName :: ByteString
testFudgeEncodedWithConsName = "{\"fudgey\":\"Chocolate\"}"

tCase
  :: TestName
  -> Encoder' a
  -> a
  -> ByteString
  -> TestTree
tCase nm enc a =
  testCase nm . assertEqual nm (E.simplePureEncodeNoSpaces enc a)

encoderTests :: TestTree
encoderTests = testGroup "Encoder"
  [ tCase "Encode Image" encodeImage testImageDataType testImageEncodedNoSpaces
  , tCase "Encode Image (Generic)" enc testImageDataType testImageEncodedNoSpaces
  , tCase "Encode newtype - with constructor name" enc testFudge testFudgeEncodedWithConsName
  ]
  where
    enc = proxy mkEncoder (Proxy :: Proxy GWaarg)
