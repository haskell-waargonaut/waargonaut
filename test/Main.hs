{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either                 (isLeft)

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy.Char8  as BSL8

import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text

import           Hedgehog
import           Text.Parsec                 (ParseError)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Waargonaut                  (Json)
import qualified Waargonaut                  as W

import           Waargonaut.Types.JNumber    (naturalDigits, naturalFromDigits)
import qualified Waargonaut.Types.Whitespace as WS

import           Types.Common                (genNatural)
import qualified Types.Json                  as J

import qualified Utils

prop_natural_digits_roundtrip :: Property
prop_natural_digits_roundtrip = property $ do
  n <- forAll genNatural
  naturalFromDigits (naturalDigits n) === Just n

prop_gen_json_tripping :: Property
prop_gen_json_tripping = withTests 5000 . property $
  forAll J.genJson >>= (\j -> tripping j encodeText decode)

prop_gen_json_draft_print_parse_print_id :: Property
prop_gen_json_draft_print_parse_print_id = withTests 5000 . property $ do
  printedA <- forAll $ encodeText <$> J.genJson
  Right printedA === (encodeText <$> decode printedA)

encodeText
  :: Json
  -> Text
encodeText =
  Text.decodeUtf8 .
  encodeByteString

encodeByteString
  :: Json
  -> ByteString
encodeByteString =
  BSL8.toStrict .
  BB.toLazyByteString .
  W.jsonBuilder WS.wsBuilder

decode
  :: Text
  -> Either ParseError Json
decode =
  Utils.testparse W.simpleWaargonaut

properties :: TestTree
properties = testGroup "Property Tests"
  [ testProperty
      "Round Trip: Natural <-> NonEmpty Digits"
      prop_natural_digits_roundtrip

  , testProperty
      "Using Waargonaut Types: parse . print = id"
      prop_gen_json_tripping

  , testProperty
      "Using Waargonaut Types: print . parse . print = print"
      prop_gen_json_draft_print_parse_print_id
  ]

parsePrint :: ByteString -> Either ParseError ByteString
parsePrint = fmap encodeByteString . decode . Text.decodeUtf8

testFile :: FilePath -> Assertion
testFile fp = do
  s <- BS8.readFile fp
  parsePrint s @?= Right s

testFileFailure :: FilePath -> Assertion
testFileFailure fp = do
  s <- BS8.readFile fp
  assertBool (fp <> " should fail to parse!") (isLeft $ parsePrint s)

unitTests :: TestTree
unitTests = testGroup "Unit Tests (print . parse = id)"
  [ testCase "test1.json" (testFile "test/test1.json")
  , testCase "test2.json" (testFile "test/test2.json")
  , testCase "test3.json" (testFile "test/test3.json")
  , testCase "test5.json" (testFile "test/test5.json")
  , testCase "test7.json" (testFile "test/test7.json")
  , testCase "twitter100.json" (testFile "test/json-data/twitter100.json")
  , testCase "jp100.json" (testFile "test/json-data/jp100.json")
  ]

regressionTests :: TestTree
regressionTests = testGroup "Regression Tests - Failure to parse = Success"
  [ testCase "[11 12 13] (test4.json)" (testFileFailure "test/test4.json")
  , testCase "{\"foo\":3\"bar\":4} (test6.json)" (testFileFailure "test/test6.json")
  ]

main :: IO ()
main = defaultMain $ testGroup "Waargonaut All Tests"
  [ properties
  , unitTests
  , regressionTests
  ]
