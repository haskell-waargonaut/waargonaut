{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Encoder
  ( encoderTests
  , encodeImage
  , testImageDataType
  ) where

import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (assertEqual, testCase)

import           Waargonaut.Encode    (Encoder, Encoder')
import qualified Waargonaut.Encode    as E

import           Data.ByteString.Lazy (ByteString)

import           Types.Common         (Fudge, Image (..), testFudge,
                                       testImageDataType)

import           Data.Tagged          (Tagged, untag)
import           Waargonaut.Generic   (GWaarg, mkEncoder)

testImageEncodedNoSpaces :: ByteString
testImageEncodedNoSpaces = "{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"

testImageGenericEncode :: ByteString
testImageGenericEncode = "{\"Animated\":false,\"Height\":600,\"IDs\":[116,943,234,38793],\"Title\":\"View from 15th Floor\",\"Width\":800}"

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
  , tCase "Encode Image (Generic)" (untag imgE) testImageDataType testImageGenericEncode
  , tCase "Encode newtype - with constructor name" (untag fudgeE) testFudge testFudgeEncodedWithConsName
  ]
  where
    imgE :: Applicative f => Tagged GWaarg (Encoder f Image)
    imgE = mkEncoder

    fudgeE :: Applicative f => Tagged GWaarg (Encoder f Fudge)
    fudgeE = mkEncoder

