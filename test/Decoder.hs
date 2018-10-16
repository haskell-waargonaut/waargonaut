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

import qualified Waargonaut.Decode.Succinct as D

import           Types.Common               (Image, imageDecodeSuccinct,
                                             parseBS, testImageDataType)

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

decodeTest1Json :: IO (Either (DecodeError, D.CursorHistory) Image)
decodeTest1Json = D.runPureDecode imageDecodeSuccinct parseBS . D.mkCursor
  <$> BS.readFile "test/json-data/test1.json"

decodeTest2Json :: Either (DecodeError, D.CursorHistory) [Int]
decodeTest2Json = D.runPureDecode mkDecoder parseBS (D.mkCursor "[23,44]")

decodeTest3Json :: Either (DecodeError, D.CursorHistory) (Char,String,[Int])
decodeTest3Json = D.runPureDecode decoder parseBS (D.mkCursor "[\"a\",\"fred\",1,2,3,4]")
  where
    decoder :: Monad f => D.Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down >=> \fstElem -> liftA3 (,,)
      (D.focus D.unboundedChar fstElem)
      (D.moveRight1 fstElem >>= D.focus D.string)
      (D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int)

