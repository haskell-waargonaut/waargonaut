{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Encoder.Laws (encoderLaws) where

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           Data.ByteString.Lazy       (ByteString)
import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Identity      (Identity)

import           Hedgehog
import qualified Hedgehog.Function          as Fn
import qualified Hedgehog.Gen               as Gen

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import qualified Laws

runSE :: ShowEncoder a -> a -> ByteString
runSE (SE e) = E.simplePureEncodeNoSpaces e

newtype ShowEncoder a = SE (Encoder Identity a)

instance Show a => Show (ShowEncoder a) where
  show (SE _) = "an encoder of type a"

instance Fn.Contravariant ShowEncoder where
  contramap f (SE a) = SE (Fn.contramap f a)

genShowEncoder :: Encoder Identity a -> Gen a -> Gen (ShowEncoder a)
genShowEncoder enc _ = Gen.constant (SE enc)

encoderLaws :: TestTree
encoderLaws = testGroup "Encoder Laws"
  [ testGroup "Contravariant"
    [ testProperty "composition"
      $ Laws.contravariant_composition_with_run (genShowEncoder E.bool) runSE Gen.bool (Gen.maybe Gen.bool) Gen.bool
    , testProperty "identity"
      $ Laws.contravariant_identity_with_run (genShowEncoder E.bool) runSE Gen.bool
    ]
  ]
