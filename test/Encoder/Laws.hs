{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Encoder.Laws (encoderLaws) where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hedgehog   (testProperty)

import           Data.ByteString.Lazy  (ByteString)
import           Data.Functor.Identity (Identity)

import           Hedgehog
import           Hedgehog.Function     (Arg, Vary)
import qualified Hedgehog.Function     as Fn
import qualified Hedgehog.Gen          as Gen

import           Waargonaut.Encode     (Encoder)
import qualified Waargonaut.Encode     as E

runSE :: ShowEncoder a -> a -> ByteString
runSE (SE e) = E.simplePureEncodeNoSpaces e

newtype ShowEncoder a = SE (Encoder Identity a)

instance Show a => Show (ShowEncoder a) where
  show (SE _) = "an encoder of type a"

instance Fn.Contravariant ShowEncoder where
  contramap f (SE a) = SE (Fn.contramap f a)

-- |
-- contravariant
--
--     contramap f . contramap g = contramap (g . f)
contravariant_composition
  :: forall f a.
     ( Show f, Arg f, Vary f, Eq f
     , Show a, Arg a, Vary a
     )
  => Gen f
  -> Gen Bool
  -> Gen a
  -> Property
contravariant_composition genF genG genA = property $ do
  f <- Fn.forAllFn $ Fn.fn genF
  g <- Fn.forAllFn $ Fn.fn genG

  let ea = SE E.bool

  a <- forAll genA

  runSE (Fn.contramap f $ Fn.contramap g ea) a === runSE (Fn.contramap (g . f) ea) a

contravariant_identity :: Property
contravariant_identity = property $ do
  a <- forAll Gen.bool

  let ea = SE E.bool

  runSE (Fn.contramap id ea) a === runSE ea a

encoderLaws :: TestTree
encoderLaws = testGroup "Encoder Laws"
  [ testProperty "Contravariant 'composition'" $ contravariant_composition Gen.bool Gen.bool Gen.bool
  , testProperty "Contravariant 'identity'" contravariant_identity
  ]
