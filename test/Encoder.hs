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
encodeImage = E.encodeAsMapLikeObj $ \img ->
  E.intAt "Width" (_imageW img) .
  E.intAt "Height" (_imageH img) .
  E.textAt "Title" (_imageTitle img) .
  E.boolAt "Animated" (_imageAnimated img) .
  E.arrayAt E.encodeInt "IDs" (_imageIDs img)

-- kv :: (At s, IxValue s ~ Json) => Encoder a -> Index s -> a -> h :>> s -> h :>> s
-- kv enc k v z = z & Z.downward (at k) & Z.focus ?~ runEncoder enc v & Z.upward

-- intAt :: Text -> Int -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- intAt = kv encodeInt

-- textAt :: Text -> Text -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- textAt = kv encodeText

-- boolAt :: Text -> Bool -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- boolAt = kv encodeBool

-- arrayAt :: (At s, Foldable f, IxValue s ~ Json) => Encoder a -> Index s -> f a -> h :>> s -> h :>> s
-- arrayAt enc = kv (encodeArray enc)

-- obj :: (Z.Zipped h a ~ MapLikeObj WS Json, Z.Zipping h a) => Zipper h i a -> Json
-- obj o = _JObj # (fromMapLikeObj $ Z.rezip o, mempty)

-- encodeImageZip :: Image -> Z.Top :>> MapLikeObj WS Json
-- encodeImageZip i = mapLikeObj
--   & intAt "Width" (_imageW i)
--   & intAt "Height" (_imageH i)
--   & textAt "Title" (_imageTitle i)
--   & boolAt "Animated" (_imageAnimated i)
--   & arrayAt encodeInt "IDs" (_imageIDs i)

-- encodeSomething :: Image -> [Int] -> Json
-- encodeSomething i xs = mapLikeObj
--   & Z.downward (at "image") & Z.focus ?~ obj (encodeImageZip i) & Z.upward
--   & Z.downward (at "ints") & Z.focus ?~ runEncoder (encodeArray encodeInt) xs
--   & obj
