{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where

import           Control.Applicative   (liftA3)
import           Control.Monad         ((>=>))

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase)

import           Data.Either           (isLeft)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Waargonaut.Generic (mkDecoder)

import           Waargonaut.Decode     (Decoder, Err)
import qualified Waargonaut.Decode     as D
import qualified Waargonaut.Types      as WT

import qualified Text.Parsec           as P

import           Types.Common          (Image (Image))

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)"
    $ decodeTest1Json >>= assertBool "Decode Image Zipper - Success" . not . isLeft

  , testCase "Decode [Int]"
    $ assertBool "[Int] Decode Success" (not $ isLeft decodeTest2Json)

  , testCase "Decode (Char,String,[Int])"
    $ assertBool "(Char,String,[Int]) Decode Success" (not $ isLeft decodeTest3Json)
  ]

parseBS :: ByteString -> Either P.ParseError WT.Json
parseBS = P.parse WT.parseWaargonaut "ByteString"

decodeTest2Json :: Either (Err P.ParseError) [Int]
decodeTest2Json = D.simpleDecode parseBS mkDecoder "[23,44]"

decodeTest3Json :: Either (Err P.ParseError) (Char,String,[Int])
decodeTest3Json = D.simpleDecode parseBS decoder "[\"a\",\"fred\",1,2,3,4]"
  where
    decoder :: Monad f => Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down "array" >=> \fstElem ->
      liftA3 (,,)
        (D.focus D.unboundedChar fstElem)
        (D.moveRight1 fstElem >>= D.focus D.string)
        (D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int)

imageDecoder :: Monad f => Decoder f Image
imageDecoder = D.withCursor $ \curs -> do
  -- We're at the root of our object, move into it and move to the value at the "Image" key
  o <- D.moveToKey "Image" curs
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int o
    <*> D.fromKey "Height" D.int o
    <*> D.fromKey "Title" D.text o
    <*> D.fromKey "Animated" D.boolean o
    <*> D.fromKey "IDs" (D.list D.int) o


decodeTest1Json :: IO (Either (Err P.ParseError) Image)
decodeTest1Json = D.simpleDecode parseBS imageDecoder <$> BS8.readFile "test/json-data/test1.json"

