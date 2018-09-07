{-# LANGUAGE OverloadedStrings #-}
module Encoder
  ( encoderTests
  , encodeImage
  , testImageDataType
  ) where

import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertEqual, testCase)

import           Waargonaut.Encode    (Encoder)
import qualified Waargonaut.Encode    as E

import           Data.ByteString.Lazy (ByteString)

import           Types.Common         (Image (..))

testImageDataType :: Image
testImageDataType = Image 800 600 "View from 15th Floor" False [116, 943, 234, 38793]

testImageEncodedNoSpaces :: ByteString
testImageEncodedNoSpaces = "{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"

encoderTests :: TestTree
encoderTests = testGroup "Encoder"
  [ testCase "Encode Image (No whitespace - default)" $
      assertEqual "Encoding Image" (E.runPureEncoder encodeImage testImageDataType) testImageEncodedNoSpaces
  ]

encodeImage :: Encoder Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageW img)
  . E.intAt "Height" (_imageH img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.arrayAt E.int "IDs" (_imageIDs img)
