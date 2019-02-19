{-# LANGUAGE RankNTypes #-}
module Json
  ( jsonTests
  , jsonPrisms
  ) where

import           Control.Applicative        (liftA2)
import           Control.Lens               (Prism', preview, review, _Empty)

import           Test.Tasty
import           Test.Tasty.Hedgehog        (testProperty)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Hedgehog                   (Gen, MonadGen, Property, forAll,
                                             property, (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy          as HM

import           Data.Scientific            (Scientific)
import qualified Data.Scientific            as Sci
import           Data.Text                  (Text)

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text       as AT

import           Waargonaut.Types

import           Waargonaut                 (_ArrayOf, _Bool, _Number,
                                             _ObjLazyHashMapOf, _String,
                                             _TextJson)

import           Types.Json                 (genJson)

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

objHashMapPrismLaws :: TestTree
objHashMapPrismLaws = testGroup "Json lazy hashmap prism"
  [ testProperty "_ObjLazyHashMapOf Scientific"
    $ prismLaw (genHashMapOf $ genScientific (Just 10)) (_ObjLazyHashMapOf _Number)

  , testProperty "_ObjLazyHashMapOf Text"
    $ prismLaw (genHashMapOf genUnicodeText) (_ObjLazyHashMapOf _String)

  , testProperty "_ObjLazyHashMapOf Bool"
    $ prismLaw (genHashMapOf Gen.bool) (_ObjLazyHashMapOf _Bool)

  , testProperty "_ObjLazyHashMapOf Json"
    $ prismLaw (genHashMapOf genJson) (_ObjLazyHashMapOf (_String . _TextJson AT.parseOnly))
  ]

arrayOfPrismLaws :: TestTree
arrayOfPrismLaws = testGroup "Json array prism"
  [ testProperty "_ArrayOf Scientific"
    $ prismLaw (genListOf $ genScientific (Just 10)) (_ArrayOf _Number)

  , testProperty "_ArrayOf Text"
    $ prismLaw (genListOf genUnicodeText) (_ArrayOf _String)

  , testProperty "_ArrayOf Bool"
    $ prismLaw (genListOf Gen.bool) (_ArrayOf _Bool)

  , testProperty "_ArrayOf Json"
    $ prismLaw (genListOf genJson) (_ArrayOf (_String . _TextJson AT.parseOnly))
  ]

jsonPrisms :: TestTree
jsonPrisms =
  testGroup "Json 'Prisms'' - OBEY THE LAW (preview p (review p x) == Just x)"
  [ testProperty "_Bool"
    $ prismLaw Gen.bool _Bool

  , testProperty "_Number"
    $ prismLaw (genScientific (Just 10)) _Number

  , testProperty "_TextJson"
    $ prismLaw genJson (_TextJson AT.parseOnly)

  , testProperty "_String"
    $ prismLaw genUnicodeText _String

  , arrayOfPrismLaws
  , objHashMapPrismLaws
  ]

genHashMapOf :: Gen a -> Gen (HashMap Text a)
genHashMapOf genElem = HM.fromList <$> genListOf tup
  where tup = liftA2 (,) genUnicodeText genElem

genListOf :: Gen a -> Gen [a]
genListOf genElem = Gen.list (Range.linear 0 100) genElem

genUnicodeText :: Gen Text
genUnicodeText = Gen.text (Range.linear 0 100) Gen.unicode
