{-# LANGUAGE RankNTypes #-}
module Json
  ( jsonTests
  , jsonPrisms
  ) where

import           Control.Lens        (Prism', preview, review, _Empty)

import           Test.Tasty
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit    (testCase, (@?=))

import           Hedgehog            (Gen, MonadGen, Property, forAll, property,
                                      (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Data.Scientific     (Scientific)
import qualified Data.Scientific     as Sci

import           Waargonaut.Types

import           Waargonaut          (_ArrayOf, _Bool, _Number)

jsonTests :: TestTree
jsonTests =
  testGroup "Json types"
    [ testCase "CommandSepareted's _Empty prism law"
      $ preview _Empty (review _Empty () :: CommaSeparated WS (JAssoc WS Json)) @?= Just ()
    , testCase "JObject's _Empty prism law"
      $ preview _Empty (review _Empty () :: JObject WS Json) @?= Just ()
    , testCase "JArray's _Empty prism law"
      $ preview _Empty (review _Empty () :: JArray WS Json) @?= Just ()
    , testCase "MapLikeObj's _Empty prism law"
      $ preview _Empty (review _Empty () :: MapLikeObj WS Json) @?= Just ()
    , testCase "WS's _Empty prism law"
      $ preview _Empty (review _Empty () :: WS) @?= Just ()
    ]

prismLaw
  :: ( Eq a
     , Show a
     )
  => Gen a
  -> Prism' b a
  -> Property
prismLaw genA prismA = property $ do
  a <- forAll genA
  preview prismA (review prismA a) === Just a

genScientific :: MonadGen m => Maybe Int -> m Scientific
genScientific lim = either fst fst . Sci.fromRationalRepetend lim
  <$> Gen.realFrac_ (Range.linearFrac 0.0001 1000.0)

jsonPrisms :: TestTree
jsonPrisms =
  testGroup "Json Prisms"
  [ testProperty "_Bool"
    $ prismLaw Gen.bool _Bool

  , testProperty "_Number"
    $ prismLaw (genScientific (Just 10)) _Number

  , testProperty "_ArrayOf"
    $ prismLaw (Gen.list (Range.linear 0 100) (genScientific (Just 10))) (_ArrayOf _Number)

  -- , testProperty "_ByteStringJson"
  --   $ prismLaw genJson (_ByteStringJson parseBS)
  ]
