{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where

import           Prelude                    (Char, Int, String, print)

import           Control.Applicative        (liftA3, (<$>), (<*>))
import           Control.Category           ((.))
import           Control.Monad              (Monad, (>=>), (>>=))

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual,
                                             assertFailure, testCase)

import           Data.Bool                  (not)
import qualified Data.ByteString            as BS
import           Data.Either                (Either)
import qualified Data.Either                as Either
import           Data.Function              (($))
import           System.IO                  (IO)

import           Waargonaut.Generic         (mkDecoder)

import           Waargonaut.Decode.Error    (DecodeError)
import           Waargonaut.Decode.Internal (ppCursorHistory)

import           Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Succinct as SD

import           Types.Common               (Image (Image), parseBS,
                                             testImageDataType, imageDecodeSuccinct)

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)"
    $ decodeTest1Json >>= Either.either failWithHistory (assertEqual "?" testImageDataType)

  , testCase "Decode [Int]"
    $ assertBool "[Int] Decode Success" (not $ Either.isLeft decodeTest2Json)

  , testCase "Decode (Char,String,[Int])"
    $ assertBool "(Char,String,[Int]) Decode Success" (not $ Either.isLeft decodeTest3Json)
  ]
  where
    failWithHistory (err, hist) = do
      print err
      print (ppCursorHistory hist)
      assertFailure "Decode Failed :("

decodeTest2Json :: Either (D.Err DecodeError) [Int]
decodeTest2Json = D.simpleDecode parseBS mkDecoder "[23,44]"

decodeTest3Json :: Either (D.Err DecodeError) (Char,String,[Int])
decodeTest3Json = D.simpleDecode parseBS decoder "[\"a\",\"fred\",1,2,3,4]"
  where
    decoder :: Monad f => D.Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down "array" >=> \fstElem -> liftA3 (,,)
      (D.focus D.unboundedChar fstElem)
      (D.moveRight1 fstElem >>= D.focus D.string)
      (D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int)

-- imageDecoder :: Monad f => SD.Decoder f Image
-- imageDecoder = SD.withCursor $ SD.down >=> \curs -> do
--   -- Move to the value at the "Image" key
--   io <- SD.moveToKey "Image" curs >>= SD.down
--   -- We need individual values off of our object,
--   Image
--     <$> SD.fromKey "Width" SD.int io
--     <*> SD.fromKey "Height" SD.int io
--     <*> SD.fromKey "Title" SD.text io
--     <*> SD.fromKey "Animated" SD.bool io
--     <*> SD.fromKey "IDs" (SD.list SD.int) io

decodeTest1Json :: IO (Either (DecodeError, SD.CursorHistory) Image)
decodeTest1Json = SD.runPureDecode imageDecodeSuccinct parseBS . SD.mkCursor
  <$> BS.readFile "test/json-data/test1.json"

