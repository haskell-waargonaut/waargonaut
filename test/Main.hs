{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either                 (isLeft)

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy.Char8  as BSL8

import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text

import           Hedgehog
import           Text.Parsec                 (ParseError, Parsec)

import qualified Hedgehog.Gen                as Gen

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Waargonaut                  (jsonBuilder, parseJsonBool,
                                              parseJsonNull, simpleParseJson)

import           Waargonaut.Types.JNumber    (naturalDigits, naturalFromDigits)
import           Waargonaut.Types.Whitespace (wsBuilder)

import           Types.Common                (genNatural, genText)

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
  isLeft (Utils.testparse pa t) === True

prop_parse_null_term_only :: Property
prop_parse_null_term_only = prop_parse_except
  (/= "null")
  (parseJsonNull (return ()))

prop_parse_bool_term_only :: Property
prop_parse_bool_term_only = prop_parse_except
  (\x -> x /= "true" && x /= "false")
  (parseJsonBool (return ()))

printParse :: ByteString -> Either ParseError ByteString
printParse o = BSL8.toStrict . BB.toLazyByteString . jsonBuilder wsBuilder
  <$> Utils.testparse simpleParseJson (Text.decodeUtf8 o)

properties :: TestTree
properties = testGroup "Property Tests"
  [ testProperty "Round Trip: Natural <-> NonEmpty Digits" prop_natural_digits_roundtrip
  , testProperty "parseJsonNull 'null' parse only" prop_parse_null_term_only
  , testProperty "parseJsonBool 'true'/'false' parse only" prop_parse_bool_term_only
  ]

testFile1 :: Assertion
testFile1 = do
  s <- BS8.readFile "test/test1.json"
  printParse s @?= Right s

testFile2 :: Assertion
testFile2 = do
  s <- BS8.readFile "test/test2.json"
  printParse s @?= Right s

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Round Trip on Test File 1 test1.json" testFile1
  , testCase "Round Trip on Test File 2 test2.json" testFile2
  ]

main :: IO ()
main = defaultMain $ testGroup "Waargonaut All Tests"
  [ properties
  , unitTests
  ]
