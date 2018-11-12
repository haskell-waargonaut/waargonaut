{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Decoder.Laws (decoderLaws) where

import           Control.Applicative     (liftA3, empty, pure, (<|>))

import           Data.Functor.Identity   (Identity)

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.Hedgehog     (testProperty)

import           Hedgehog
-- import qualified Hedgehog.Function       as Fn
import qualified Hedgehog.Gen            as Gen
-- import qualified Hedgehog.Range          as Range

import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (DecodeError)
import           Waargonaut.Decode.Types (Decoder)

import           Types.Common            (parseBS)

runD :: Decoder Identity a -> Either (DecodeError, D.CursorHistory) a
runD d = D.runPureDecode d parseBS (D.mkCursor "true")

-- identity
decoder_alternative_id :: Property
decoder_alternative_id = property $ do
  a <- forAll Gen.bool

  runD (empty <|> pure a) === pure a
  runD (pure a <|> empty) === pure a

-- associativity
decoder_alternative_associativity :: Property
decoder_alternative_associativity = property $ do
  (a,b,c) <- forAll (liftA3 (,,) Gen.bool Gen.bool Gen.bool)

  let
    dA = pure a
    dB = pure b
    dC = pure c

  -- (a <|> b) <|> c = a <|> (b <|> c)
  runD ((dA <|> dB) <|> dC) === runD (dA <|> (dB <|> dC))

-- | Applicative Laws
--
-- identity
--
--     pure id <*> v = v
applicative_id :: Property
applicative_id = property $ do
  a <- pure <$> forAll Gen.bool

  pure id <*> pure a ==


--
-- composition
--
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- homomorphism
--
--     pure f <*> pure x = pure (f x)
--
-- interchange
--
--     u <*> pure y = pure ($ y) <*> u

decoderLaws :: TestTree
decoderLaws = testGroup "Decoder Laws"
  [ testProperty "Alternative 'identity'" decoder_alternative_id
  , testProperty "Alternative 'associativity'" decoder_alternative_associativity
  ]
