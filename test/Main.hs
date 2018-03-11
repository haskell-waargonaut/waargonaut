module Main where

import           Numeric.Natural     (Natural)

import           Hedgehog

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Test.Tasty
import           Test.Tasty.Hedgehog

import qualified Waargonaut          as Waarg

genNatural :: Gen Natural
genNatural = fmap fromIntegral <$> Gen.filter (>= 0) $ Gen.int Range.constantBounded

prop_natural_digits_roundtrip :: Property
prop_natural_digits_roundtrip = property $ do
  n <- forAll genNatural
  Waarg.naturalFromDigits (Waarg.naturalDigits n) === Just n

main :: IO ()
main = defaultMain $
  testGroup "Waargonaut Tests"
  [ testProperty "Round Trip: Natural <-> NonEmpty Digits" prop_natural_digits_roundtrip
  ]
