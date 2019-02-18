{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Encoder
  ( encoderTests
  , encodeImage
  , testImageDataType
  ) where

import           Control.Lens          ((<&>), (?~))

import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Data.Proxy            (Proxy (..))

import           Waargonaut.Encode     (Encoder, Encoder')
import qualified Waargonaut.Encode     as E

import           Data.Text.Lazy  (Text)

import           Types.Common          (Image (..), Overlayed (..), testFudge,
                                        testImageDataType)

import           Waargonaut.Generic    (GWaarg, mkEncoder, proxy)
import           Waargonaut.Types.Json (oat)

testImageEncodedNoSpaces :: Text
testImageEncodedNoSpaces = "{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"

-- | The recommended way of defining an Encoder is to be explicit.
encodeImage :: Applicative f => Encoder f Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageWidth img)
  . E.intAt "Height" (_imageHeight img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.listAt E.int "IDs" (_imageIDs img)

testFudgeEncodedWithConsName :: Text
testFudgeEncodedWithConsName = "{\"fudgey\":\"Chocolate\"}"

testOverlayed :: Overlayed
testOverlayed = Overlayed "fred" testFudge

testOverlayedOut :: Text
testOverlayedOut = "{\"id\":\"fred\",\"fudgey\":\"Chocolate\"}"

encodeOverlay :: Applicative f => Encoder f Overlayed
encodeOverlay = E.encodeA $ \(Overlayed i f) -> E.runEncoder fudgeEnc f
  <&> oat "id" ?~ E.runPureEncoder E.text i
  where
    fudgeEnc = proxy mkEncoder (Proxy :: Proxy GWaarg)

tCase
  :: TestName
  -> Encoder' a
  -> a
  -> Text
  -> TestTree
tCase nm enc a expected = testCase nm $
  E.simplePureEncodeTextNoSpaces enc a @?= expected

encoderTests :: TestTree
encoderTests = testGroup "Encoder"
  [ tCase "Image" encodeImage testImageDataType testImageEncodedNoSpaces
  , tCase "Image (Generic)" enc testImageDataType testImageEncodedNoSpaces
  , tCase "newtype - with constructor name" enc testFudge testFudgeEncodedWithConsName
  , tCase "Overlayed" encodeOverlay testOverlayed testOverlayedOut
  ]
  where
    enc = proxy mkEncoder (Proxy :: Proxy GWaarg)
