{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either              (isLeft)

import           Data.Text                (Text)

import           Hedgehog
import           Text.Parsec              (Parsec)

import qualified Hedgehog.Gen             as Gen

import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Waargonaut               (parseJsonBool, parseJsonNull)

import           Waargonaut.Types.JNumber (naturalDigits, naturalFromDigits)

import           Types.Common             (genNatural, genText)

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

properties :: TestTree
properties = testGroup "Waargonaut Tests"
  [ testProperty "Round Trip: Natural <-> NonEmpty Digits" prop_natural_digits_roundtrip
  , testProperty "parseJsonNull 'null' parse only" prop_parse_null_term_only
  , testProperty "parseJsonBool 'true'/'false' parse only" prop_parse_bool_term_only
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ properties
  -- add unit tests later I guess
  ]
