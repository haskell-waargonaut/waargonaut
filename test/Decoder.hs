{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where

import           Prelude                    (Char, Eq, Int, Show, String, print,
                                             (==))

import           Control.Applicative        (liftA3, pure, (<$>), (<|>))
import           Control.Category           ((.))
import           Control.Monad              (Monad, (>=>), (>>=))
import           Control.Monad.Except       (throwError)

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertBool, assertEqual,
                                             assertFailure, testCase, (@?=))

import qualified Data.ByteString            as BS
import qualified Data.Either                as Either
import           Data.Function              (const, ($))
import           Data.Tagged                (untag)

import           Waargonaut.Generic         (mkDecoder)

import           Waargonaut.Decode.Internal (ppCursorHistory)

import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D

import           Types.Common               (imageDecodeSuccinct, parseBS,
                                             testImageDataType)

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)" decodeTest1Json
  , testCase "Decode [Int]" decodeTest2Json
  , testCase "Decode (Char,String,[Int])" decodeTest3Json
  , testCase "Decode Fail with Bad Key" decodeTestBadObjKey
  , testCase "Decode Fail with Missing Key" decodeTestMissingObjKey
  , testCase "Decode Enum and throwError" decodeTestEnumError
  , testCase "Decode Using Alternative" decodeAlternative
  ]

decodeTestMissingObjKey :: Assertion
decodeTestMissingObjKey = do
  let
    j = "{\"foo\":33}"

    d = D.withCursor $ D.down >=> D.fromKey "bar" D.int

  r <- D.runDecode d parseBS (D.mkCursor j)

  Either.either
    (\(e, _) -> assertBool "Incorrect Error - Expected KeyDecodeFailed" (e == D.KeyNotFound "bar") )
    (\_ -> assertFailure "Expected Error!")
    r

decodeTestBadObjKey :: Assertion
decodeTestBadObjKey = do
  let
    j = "{33:33}"

    d = D.withCursor $ D.down >=> D.fromKey "foo" D.int

  r <- D.runDecode d parseBS (D.mkCursor j)

  Either.either
    (\(e, _) -> assertBool "Incorrect Error - Expected KeyDecodeFailed" (e == D.KeyDecodeFailed) )
    (\_ -> assertFailure "Expected Error!")
    r

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
    listDecode = untag mkDecoder

decodeTest3Json :: Assertion
decodeTest3Json = assertBool "(Char,String,[Int]) Decode Success" . Either.isRight
  $ D.runPureDecode decoder parseBS (D.mkCursor "[\"a\",\"fred\",1,2,3,4]")
  where
    decoder :: Monad f => D.Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down >=> \fstElem -> liftA3 (,,)
      (D.focus D.unboundedChar fstElem)
      (D.moveRight1 fstElem >>= D.focus D.string)
      (D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int)

data MyEnum
  = A
  | B
  | C
  deriving (Eq, Show)

decodeMyEnum :: Monad f => D.Decoder f MyEnum
decodeMyEnum = D.text >>= \case
  "a" -> pure A
  "b" -> pure B
  "c" -> pure C
  _   -> throwError (D.ConversionFailure "MyEnum")

decodeTestEnumError :: Assertion
decodeTestEnumError = D.runDecode decodeMyEnum parseBS (D.mkCursor "\"WUT\"")
  >>= Either.either
    (\(e, _) -> assertBool "Incorrect Error!" (e == D.ConversionFailure "MyEnum"))
    (const (assertFailure "Should not succeed"))

decodeAlternative :: Assertion
decodeAlternative = do
  t <- D.runDecode d parseBS (D.mkCursor "\"FRED\"")
  i <- D.runDecode d parseBS (D.mkCursor "33")

  t @?= Either.Right (Either.Left "FRED")
  i @?= Either.Right (Either.Right 33)
  where
    d = (Either.Left <$> D.text) <|> (Either.Right <$> D.int)
