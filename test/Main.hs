{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either                 (isLeft)

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy.Char8  as BSL8

import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text

import           Data.Digit                  (Digit)

import           Hedgehog
import           Text.Parsec                 (ParseError, Parsec)

import qualified Hedgehog.Gen                as Gen

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Waargonaut                  (Json, jsonBuilder, parseJsonBool,
                                              parseJsonNull, simpleParseJson)

import           Waargonaut.Types.JNumber    (naturalDigits, naturalFromDigits)
import           Waargonaut.Types.Whitespace (WS, wsBuilder)

import qualified Types.JsonDraft             as JD
import qualified WaargDraft                  as WD

import           Types.Common                (genNatural, genText)

import           Types.Json

import qualified Utils

prop_natural_digits_roundtrip :: Property
prop_natural_digits_roundtrip = property $ do
  n <- forAll genNatural
  naturalFromDigits (naturalDigits n) === Just n

prop_parse_except
  :: (Text -> Bool)
  -> Parsec Text () a
  -> Property
prop_parse_except fi pa = property $ do
  t <- forAll (Gen.filter fi genText)
  Hedgehog.assert . isLeft . Utils.testparse pa $ t

prop_parse_null_term_only :: Property
prop_parse_null_term_only = prop_parse_except
  (/= "null")
  (parseJsonNull (return ()))

prop_parse_bool_term_only :: Property
prop_parse_bool_term_only = prop_parse_except
  (\x -> x /= "true" && x /= "false")
  (parseJsonBool (return ()))

printParse :: ByteString -> Either ParseError ByteString
printParse o = BSL8.toStrict . BB.toLazyByteString . WD.jsonBuilder wsBuilder
  <$> Utils.testparse WD.simpleWaargDraft (Text.decodeUtf8 o)

prop_gen_json_tripping :: Property
prop_gen_json_tripping = withTests 1000 . property $ do
  j <- forAll JD.genJson
  tripping j enencode dedecode
  where
    enencode =
      Text.decodeUtf8 .
      BSL8.toStrict .
      BB.toLazyByteString .
      WD.jsonBuilder wsBuilder

    dedecode = Utils.testparse WD.simpleWaargDraft

prop_gen_json_draft_print_parse_print_id :: Property
prop_gen_json_draft_print_parse_print_id = withTests 1000 . property $ do
  printedA <- forAll $ enencode <$> JD.genJson
  Right printedA === (enencode <$> dedecode printedA)
  where
    enencode =
      Text.decodeUtf8 .
      BSL8.toStrict .
      BB.toLazyByteString .
      WD.jsonBuilder wsBuilder

    dedecode = Utils.testparse WD.simpleWaargDraft

prop_gen_json_print_parse_print_id :: Property
prop_gen_json_print_parse_print_id = withTests 1000 . property $ do
  printedA <- forAll $ encode <$> genJson
  Right printedA === (encode <$> decode printedA)

decode
  :: Text
  -> Either ParseError (Json Digit WS)
decode =
  Utils.testparse simpleParseJson

encode
  :: Json Digit WS
  -> Text
encode =
  Text.decodeUtf8 .
  BSL8.toStrict .
  BB.toLazyByteString .
  jsonBuilder wsBuilder

properties :: TestTree
properties = testGroup "Property Tests"
  [ testProperty
      "Round Trip: Natural <-> NonEmpty Digits"
      prop_natural_digits_roundtrip

  , testProperty
      "parseJsonNull 'null' parse only"
      prop_parse_null_term_only

  , testProperty
      "parseJsonBool 'true'/'false' parse only"
      prop_parse_bool_term_only

  , testProperty
      "Generate DRAFT AST, round trip, compare ASTs"
      prop_gen_json_tripping

  -- , testProperty
  --     "Generate DRAFT AST -> print DRAFT AST = (print . parse DRAFT AST)"
  --     prop_gen_json_draft_print_parse_print_id

  -- , testProperty
  --     "Generate AST -> print AST = (print . parse AST)"
  --     prop_gen_json_print_parse_print_id

  ]

testFile :: FilePath -> Assertion
testFile fp = do
  s <- BS8.readFile fp
  printParse s @?= Right s

testFile1 :: Assertion
testFile1 = testFile "test/test1.json"

testFile2 :: Assertion
testFile2 = testFile "test/test2.json"

testFile3 :: Assertion
testFile3 = testFile "test/json-data/jp100.json"

testFile4 :: Assertion
testFile4 = testFile "test/json-data/twitter100.json"

testFile5 :: Assertion
testFile5 = testFile "test/test3.json"

unitTests :: TestTree
unitTests = testGroup "Unit Tests" []
  -- [ testCase "Round Trip on Test File 1 test1.json" testFile1
  -- , testCase "Round Trip on Test File 2 test2.json" testFile2
  -- , testCase "Round Trip on Test File 3 test3.json" testFile3
  -- , testCase "Round Trip on Test File 4 twitter100.json" testFile4
  -- , testCase "Round Trip on Test File 5 jp100.json" testFile5
  -- ]

main :: IO ()
main = defaultMain $ testGroup "Waargonaut All Tests"
  [ properties
  , unitTests
  ]
