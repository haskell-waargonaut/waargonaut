{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where

import           Prelude                    (Char, Int, String, print)

import           Control.Applicative        (liftA3, (<$>))
import           Control.Category           ((.))
import           Control.Monad              (Monad, (>=>), (>>=))

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertBool, assertEqual,
                                             assertFailure, testCase)

import qualified Data.ByteString            as BS
import qualified Data.Either                as Either
import           Data.Function              (($))

import           Waargonaut.Generic         (mkDecoder, unGJDec)

import           Waargonaut.Decode.Internal (ppCursorHistory)

import qualified Waargonaut.Decode          as D

import           Types.Common               (imageDecodeSuccinct, parseBS,
                                             testImageDataType)

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)" decodeTest1Json
  , testCase "Decode [Int]" decodeTest2Json
  , testCase "Decode (Char,String,[Int])" decodeTest3Json
  ]

decodeTest1Json :: Assertion
decodeTest1Json = D.runPureDecode imageDecodeSuccinct parseBS . D.mkCursor
  <$> BS.readFile "test/json-data/test1.json"
  >>= Either.either failWithHistory (assertEqual "Image Decode Failed" testImageDataType)
  where
    failWithHistory (err, hist) = do
      print err
      print (ppCursorHistory hist)
      assertFailure "Decode Failed :("

decodeTest2Json :: Assertion
decodeTest2Json = assertBool "[Int] Decode Success" . Either.isRight
  $ D.runPureDecode listDecode parseBS (D.mkCursor "[23,44]")
  where
    listDecode :: Monad f => D.Decoder f [Int]
    listDecode = unGJDec mkDecoder

decodeTest3Json :: Assertion
decodeTest3Json = assertBool "(Char,String,[Int]) Decode Success" . Either.isRight
  $ D.runPureDecode decoder parseBS (D.mkCursor "[\"a\",\"fred\",1,2,3,4]")
  where
    decoder :: Monad f => D.Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down >=> \fstElem -> liftA3 (,,)
      (D.focus D.unboundedChar fstElem)
      (D.moveRight1 fstElem >>= D.focus D.string)
      (D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int)
