{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Encoder
  ( encoderTests
  , encodeImage
  , testImageDataType
  ) where

import           Generics.SOP         (Generic, HasDatatypeInfo)
import qualified GHC.Generics         as GHC

import           Data.Text            (Text)

import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (assertEqual, testCase)

import           Waargonaut.Encode    (Encoder)
import qualified Waargonaut.Encode    as E

import           Data.ByteString.Lazy (ByteString)

import           Types.Common         (Image (..))

import           Waargonaut.Generic   (EncodeOptions (..), JsonEncode,
                                       NewtypeEncodeName (..),
                                       defaultEncodeOpts, gEncoder, mkEncoder)

testImageDataType :: Image
testImageDataType = Image 800 600 "View from 15th Floor" False [116, 943, 234, 38793]

testImageEncodedNoSpaces :: ByteString
testImageEncodedNoSpaces = "{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"

-- | The recommended way of defining an Encoder is to be explicit, the 'Generic'
-- Encoder isn't very clever and may not always behave as you like.
encodeImage :: Encoder Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageW img)
  . E.intAt "Height" (_imageH img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.listAt E.int "IDs" (_imageIDs img)

newtype Fudge = Fudge Text
  deriving (Show, GHC.Generic)

instance Generic Fudge
instance HasDatatypeInfo Fudge

-- instance JsonEncode Fudge

instance JsonEncode Fudge where
  mkEncoder = gEncoder
    (defaultEncodeOpts { _encodeNewtypeWithConstructorName = ConstructorNameAsKey
                       })

testFudge :: Fudge
testFudge = Fudge "Chocolate"

-- testFudgeEncodedUnwrapped :: ByteString
-- testFudgeEncodedUnwrapped = "\"Chocolate\""

testFudgeEncodedWithConsName :: ByteString
testFudgeEncodedWithConsName = "{\"Fudge\":\"Chocolate\"}"

tCase
  :: TestName
  -> Encoder a
  -> a
  -> ByteString
  -> TestTree
tCase nm enc a =
  testCase nm . assertEqual nm (E.runPureEncoder enc a)

encoderTests :: TestTree
encoderTests = testGroup "Encoder"
  [ tCase"Encode Image (No whitespace - default)" encodeImage testImageDataType testImageEncodedNoSpaces
  -- , tCase "Encode newtype - unwrapped" mkEncoder testFudge testFudgeEncodedUnwrapped
  , tCase "Encode newtype - with constructor name" mkEncoder testFudge testFudgeEncodedWithConsName
  ]

