{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where

import           Prelude                    (Char, Eq, Int, Show (show), String,
                                             print, (==))

import           Control.Applicative        (liftA3, pure, (<$>))
import           Control.Category           ((.))
import           Control.Lens               (preview)
import           Control.Monad              (Monad, (>=>), (>>=))

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertBool, assertEqual,
                                             assertFailure, testCase, (@?=))

import           Hedgehog                   (Property, evalIO, property,
                                             withTests, (/==), (===))
import           Test.Tasty.Hedgehog        (testProperty)

import           Data.Bool                  (Bool (..))
import qualified Data.ByteString            as BS
import qualified Data.Either                as Either
import           Data.Function              (const, ($))
import           Data.Functor               (fmap)
import           Data.Maybe                 (Maybe (Just, Nothing))
import           Data.Semigroup             (Semigroup ((<>)))
import qualified Data.Sequence              as Seq
import           Data.Tagged                (untag)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Natural                    as N

import           Waargonaut                 (_Text)

import           Waargonaut.Generic         (mkDecoder)

import           Waargonaut.Decode.Internal (ZipperMove (BranchFail),
                                             ppCursorHistory, unCursorHistory')

import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D

import           Types.Common               (imageDecodeSuccinct, parseBS,
                                             testImageDataType)

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Image (test1.json)" decodeTest1Json
  , testCase "[Int]" decodeTest2Json
  , testCase "(Char,String,[Int])" decodeTest3Json
  , testCase "Fail with Bad Key" decodeTestBadObjKey
  , testCase "Fail with Missing Key" decodeTestMissingObjKey
  , testCase "Enum" decodeTestEnum
  , testCase "Enum and throwError" decodeTestEnumError
  , testCase "Using Alt" decodeAlt
  , testCase "Using Alt (Error) - Records BranchFail" decodeAltError
  , testCase "List Decoder" listDecoder
  , testCase "NonEmpty List Decoder" nonEmptyDecoder
  , testCase "Object Decoder" objectAsKeyValuesDecoder
  , testCase "Absent Key Decoder" absentKeyDecoder
  , testCase "Either decoding order - Right first" decodeEitherRightFirst
  , testProperty "Unicode codepoint handling regression" unicodeHandlingRegression
  ]

unicodeHandlingRegression :: Property
unicodeHandlingRegression = withTests 1 . property $ do
  let
    -- Prepare a decoder function
    dec = parseBS

    -- Decoder for JSON -> Text
    decText = dec D.text
    -- Decoder for JSON -> Json
    decJson = dec D.json

    -- Manual created JSON "String" input
    manualInput  = "\"\\u2705\""

    -- what the above JSON string should parse into.
    strGood      = ("\x2705" :: String)

    -- what issue #58 claims is produced by parsing to 'Text'
    strBad       = ("\x05" :: String)

    -- This is the expected 'Text' value
    expectedText = Either.Right ("\x2705" :: Text)

  -- Read the JSON string in from a file to try to avoid differences from
  -- creating the text input via haskell values.
  fileInput <- evalIO $ BS.readFile "test/json-data/unicode_2705.json"

  let
    -- Decode the manual and file inputs to 'Text'
    fileInputDecoded = decText fileInput
    manualInputDecoded = decText manualInput

    -- Decode the manual and file inputs to their 'Json' representations
    fileInputJson = decJson fileInput
    manualInputJson = decJson manualInput

  -- Do the 'Text' decoded values match our expectations
  fileInputDecoded === expectedText
  manualInputDecoded === expectedText

  -- If we decode to 'Json' and then use the prism, do we still get the required
  -- string. There should be no reason this is different to decoding to text.
  -- Key words being "should be".
  (fmap (preview _Text) fileInputJson) === fmap pure expectedText
  (fmap (preview _Text) manualInputJson) === fmap pure expectedText

  -- For comparison, take the expected good/bad 'String' values and pack them to 'Text'.
  Either.Right (T.pack strGood) === fileInputDecoded
  Either.Right (T.pack strGood) === manualInputDecoded

  Either.Right (T.pack strBad) /== fileInputDecoded
  Either.Right (T.pack strBad) /== manualInputDecoded

nonEmptyDecoder :: Assertion
nonEmptyDecoder = do
  let
    dec = parseBS (D.nonempty D.int)

    ok = "[1]"
    notOkay = "[]"

    badTypeObj = "{}"
    badTypeText = "\"test\""
    badTypeNum = "3"

    badElem = "[1, \"fred\"]"

  assertBool "NonEmpty Decoder - fail! non-empty list decoder BROKEN. Start panicking" (Either.isRight (dec ok))
  assertBool "NonEmpty Decoder - empty list shouldn't succeed" (Either.isLeft (dec notOkay))
  assertBool "NonEmpty Decoder - invalid element decoder accepted" (Either.isLeft (dec badElem))
  assertBool "NonEmpty Decoder - invalid type accepted - object" (Either.isLeft (dec badTypeObj))
  assertBool "NonEmpty Decoder - invalid type accepted - text" (Either.isLeft (dec badTypeText))
  assertBool "NonEmpty Decoder - invalid type accepted - num" (Either.isLeft (dec badTypeNum))

listDecoder :: Assertion
listDecoder = do
  let
    dec = parseBS (D.list D.int)

    ok = "[1,2,3]"
    okE = "[]"

    badTypeObj = "{}"
    badTypeText = "\"test\""
    badTypeNum = "3"
    badElem = "[\"fred\", \"susan\"]"

  assertBool "List Decoder - fail! List Decoder BROKEN. Start panicking." (Either.isRight (dec ok))
  assertBool "List Decoder - empty list fail" (Either.isRight (dec okE))
  assertBool "List Decoder - invalid type accepted - object" (Either.isLeft (dec badTypeObj))
  assertBool "List Decoder - invalid type accepted - text" (Either.isLeft (dec badTypeText))
  assertBool "List Decoder - invalid type accepted - num" (Either.isLeft (dec badTypeNum))
  assertBool "List Decoder - invalid element decoder accepted" (Either.isLeft (dec badElem))

objectAsKeyValuesDecoder :: Assertion
objectAsKeyValuesDecoder = do
  let
    dec = parseBS (D.objectAsKeyValues D.text D.int)

    ok = "{\"1\":1,\"2\":2,\"3\":3}"
    okE = "{}"

    badTypeArray = "[]"
    badTypeText = "\"test\""
    badTypeNum = "3"
    badKey = "{1:1}"
    badValue = "{\"1\":\"fred\", \"2\":\"susan\"}"

  assertBool "Object Decoder - fail! Object Decoder BROKEN. Start panicking." (Either.isRight (dec ok))
  assertBool "Object Decoder - empty list fail" (Either.isRight (dec okE))
  assertBool "Object Decoder - invalid type accepted - object" (Either.isLeft (dec badTypeArray))
  assertBool "Object Decoder - invalid type accepted - text" (Either.isLeft (dec badTypeText))
  assertBool "Object Decoder - invalid type accepted - num" (Either.isLeft (dec badTypeNum))
  assertBool "Object Decoder - invalid key decoder accepted" (Either.isLeft (dec badKey))
  assertBool "Object Decoder - invalid value decoder accepted" (Either.isLeft (dec badValue))

decodeTestMissingObjKey :: Assertion
decodeTestMissingObjKey = do
  let
    j = "{\"foo\":33}"

    d = D.withCursor $ D.down >=> D.fromKey "bar" D.int

  let r = parseBS d j

  Either.either
    (\(e, _) -> assertBool "Incorrect Error - Expected KeyDecodeFailed" (e == D.KeyNotFound "bar"))
    (\_ -> assertFailure "Expected Error!")
    r

decodeTestBadObjKey :: Assertion
decodeTestBadObjKey = do
  let
    j = "{33:33}"

    d = D.withCursor $ D.down >=> D.fromKey "foo" D.int

  let r = parseBS d j

  Either.either
    (\(e, _) -> assertBool "Incorrect Error - Expected KeyDecodeFailed" (e == D.KeyDecodeFailed) )
    (\_ -> assertFailure "Expected Error!")
    r

decodeTest1Json :: Assertion
decodeTest1Json = parseBS imageDecodeSuccinct
  <$> BS.readFile "test/json-data/test1.json"
  >>= Either.either failWithHistory (assertEqual "Image Decode Failed" testImageDataType)
  where
    failWithHistory (err, hist) = do
      print err
      print (ppCursorHistory hist)
      assertFailure "Decode Failed :("

decodeTest2Json :: Assertion
decodeTest2Json = assertBool "[Int] Decode Success" . Either.isRight
  $ parseBS listDecode "[23,44]"
  where
    listDecode :: Monad f => D.Decoder f [Int]
    listDecode = untag mkDecoder

decodeTest3Json :: Assertion
decodeTest3Json = assertBool "(Char,String,[Int]) Decode Success" . Either.isRight
  $ parseBS decoder "[\"a\",\"fred\",1,2,3,4]"
  where
    decoder :: Monad f => D.Decoder f (Char,String,[Int])
    decoder = D.withCursor $ D.down >=> \fstElem -> liftA3 (,,)
      (D.focus D.unboundedChar fstElem)
      (D.moveRight1 fstElem >>= D.focus D.string)
      (D.moveRightN (N.successor' (N.successor' N.zero')) fstElem >>= D.rightwardSnoc [] D.int)

data MyEnum
  = A
  | B
  | C
  deriving (Eq, Show)

decodeMyEnum :: Monad f => D.Decoder f MyEnum
decodeMyEnum = D.oneOf D.text "MyEnum"
  [ ("a", A)
  , ("b", B)
  , ("c", C)
  ]

decodeTestEnum :: Assertion
decodeTestEnum = do
  chk "\"a\"" A
  chk "\"b\"" B
  chk "\"c\"" C
  where
    chk i o =
      parseBS decodeMyEnum i @?= (Either.Right o)

decodeTestEnumError :: Assertion
decodeTestEnumError =
  let
    i = parseBS decodeMyEnum "\"WUT\""
  in
    Either.either
    (\(e, _) -> assertBool "Incorrect Error!" (e == D.ConversionFailure "MyEnum"))
    (const (assertFailure "Should not succeed"))
    i

decodeEitherAlt :: Monad f => D.Decoder f (Either.Either Text Int)
decodeEitherAlt = D.either D.text D.int

decodeEitherRightFirst :: Assertion
decodeEitherRightFirst = do
  let d = parseBS (D.either D.scientific D.int)

  d "44e333" @?= Either.Right (Either.Left 44e333)
  d "33" @?= Either.Right (Either.Right 33)

decodeAlt :: Assertion
decodeAlt = do
  let
    t = parseBS decodeEitherAlt "\"FRED\""
    i = parseBS decodeEitherAlt "33"

  t @?= Either.Right (Either.Left "FRED")
  i @?= Either.Right (Either.Right 33)

decodeAltError :: Assertion
decodeAltError =
  let
    i = parseBS decodeEitherAlt "{\"foo\":33}"
  in
    Either.either
                 (\(_,h) -> assertBool "BranchFail error not found in history" $
                   case Seq.viewr (unCursorHistory' h) of
                     _ Seq.:> (BranchFail _, _) -> True
                     _                          -> False
                 )
                 (\_ -> assertFailure "Alt Error Test should fail")
                 i

absentKeyDecoder :: Assertion
absentKeyDecoder = do
  let
    a = parseBS (D.atKeyOptional "key" D.text) "{\"key\":\"present\"}"
    b = parseBS (D.atKeyOptional "missing" D.text) "{\"key\":\"present\"}"
    c = parseBS (D.atKeyOptional "key" D.int) "{\"key\":\"present\"}"

  a @?= Either.Right (Just "present")
  b @?= Either.Right Nothing
  case c of
    Either.Right _ ->
      assertFailure "atKeyOptional succeeded when it shouldn't have"
    Either.Left (e, _) ->
      assertEqual ("atKeyOptional failed incorrectly: " <> show e) e (D.ConversionFailure "integral")
