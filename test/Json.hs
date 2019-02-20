{-# LANGUAGE RankNTypes #-}
module Json
  ( jsonPrisms
  ) where

import           Control.Applicative  (liftA2)
import           Control.Lens         (Prism', preview, review, _Empty)

import           Test.Tasty
import           Test.Tasty.Hedgehog  (testProperty)
import           Test.Tasty.HUnit     (testCase, (@?=))

import           Hedgehog             (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Data.Vector          (Vector)
import qualified Data.Vector          as V

import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM

import           Data.Text            (Text)

import qualified Data.Attoparsec.Text as AT

import           Waargonaut.Types

import           Waargonaut.Lens      (_ArrayOf, _Bool, _Number, _ObjHashMapOf,
                                       _String, _TextJson)

import           Types.Common         (genScientific, genText)
import           Types.Json           (genJson)

prismLaw :: (Eq a, Show a) => Gen a -> Prism' b a -> Property
prismLaw genA prismA = property $ forAll genA >>= \a ->
  preview prismA (review prismA a) === Just a

emptyPrismLaw :: TestTree
emptyPrismLaw =
  testGroup "_Empty"
    [ testCase "CommaSeparated"
      $ preview _Empty (review _Empty () :: CommaSeparated WS (JAssoc WS Json)) @?= Just ()
    , testCase "JObject"
      $ preview _Empty (review _Empty () :: JObject WS Json) @?= Just ()
    , testCase "JArray"
      $ preview _Empty (review _Empty () :: JArray WS Json) @?= Just ()
    , testCase "MapLikeObj"
      $ preview _Empty (review _Empty () :: MapLikeObj WS Json) @?= Just ()
    , testCase "WS"
      $ preview _Empty (review _Empty () :: WS) @?= Just ()
    ]

objHashMapPrismLaws :: TestTree
objHashMapPrismLaws = testGroup "Json hashmap prism"
  [ testProperty "_ObjHashMapOf Scientific"
    $ prismLaw (genHashMapOf $ genScientific (Just 10)) (_ObjHashMapOf _Number)

  , testProperty "_ObjHashMapOf Text"
    $ prismLaw (genHashMapOf genText) (_ObjHashMapOf _String)

  , testProperty "_ObjHashMapOf Bool"
    $ prismLaw (genHashMapOf Gen.bool) (_ObjHashMapOf _Bool)

  , testProperty "_ObjHashMapOf Json"
    $ prismLaw (genHashMapOf genJson) (_ObjHashMapOf (_String . _TextJson AT.parseOnly))
  ]

arrayOfPrismLaws :: TestTree
arrayOfPrismLaws = testGroup "Json array prism"
  [ testProperty "_ArrayOf Scientific"
    $ prismLaw (genListOf $ genScientific (Just 10)) (_ArrayOf _Number)

  , testProperty "_ArrayOf Text"
    $ prismLaw (genListOf genText) (_ArrayOf _String)

  , testProperty "_ArrayOf Bool"
    $ prismLaw (genListOf Gen.bool) (_ArrayOf _Bool)

  , testProperty "_ArrayOf Json"
    $ prismLaw (genListOf genJson) (_ArrayOf (_String . _TextJson AT.parseOnly))
  ]

jsonPrisms :: TestTree
jsonPrisms =
  testGroup "Json 'Prisms'' must OBEY THE LAW: (preview p (review p x) == Just x)"
  [ testProperty "_Bool"     $ prismLaw Gen.bool _Bool
  , testProperty "_Number"   $ prismLaw (genScientific (Just 10)) _Number
  , testProperty "_TextJson" $ prismLaw genJson (_TextJson AT.parseOnly)
  , testProperty "_String"   $ prismLaw genText _String

  , arrayOfPrismLaws
  , objHashMapPrismLaws
  , emptyPrismLaw
  ]

genHashMapOf :: Gen a -> Gen (HashMap Text a)
genHashMapOf genElem = HM.fromList <$> Gen.list (Range.linear 0 100) tup
  where tup = liftA2 (,) genText genElem

genListOf :: Gen a -> Gen (Vector a)
genListOf genElem = V.fromList <$> Gen.list (Range.linear 0 100) genElem
