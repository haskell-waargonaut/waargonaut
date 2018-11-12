{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Decoder.Laws (decoderLaws) where

import           Control.Applicative     (Applicative, liftA3, pure)
import           Control.Monad.Except    (throwError)

import           Data.Functor.Alt        (Alt ((<!>)))
import           Data.Functor.Identity   (Identity)

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.Hedgehog     (testProperty)

import           Hedgehog
import           Hedgehog.Function       (Arg, Vary)
import qualified Hedgehog.Function       as Fn
import qualified Hedgehog.Gen            as Gen

import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (DecodeError (EmptyDecodeFailure))
import           Waargonaut.Decode.Types (Decoder)

import           Types.Common            (parseBS)

runD :: Decoder Identity a -> Either (DecodeError, D.CursorHistory) a
runD d = D.runPureDecode d parseBS (D.mkCursor "true")

runSD :: ShowDecoder a -> Either (DecodeError, D.CursorHistory) a
runSD = runD . unShowDecoder

newtype ShowDecoder a = SD
  { unShowDecoder :: Decoder Identity a
  }
  deriving (Functor, Monad, Applicative)

instance Alt ShowDecoder where
  (SD a) <!> (SD b) = SD (a <!> b)

instance Eq a => Eq (ShowDecoder a) where
  (SD a) == (SD b) = runD a == runD b

instance Show a => Show (ShowDecoder a) where
  show (SD d) = show $ runD d

genShowDecoder :: Gen a -> Gen (ShowDecoder a)
genShowDecoder genA = Gen.choice
  [ SD . pure <$> genA
  , SD <$> Gen.constant (throwError EmptyDecodeFailure)
  ]

-- |
-- Alt Associative
-- <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
--
alt_associativity :: Property
alt_associativity = property $ do
  (a,b,c) <- forAll $ liftA3 (,,)
    (genShowDecoder Gen.bool)
    (genShowDecoder Gen.bool)
    (genShowDecoder Gen.bool)

  runSD ((a <!> b) <!> c) === runSD (a <!> (b <!> c))

-- |
-- Alt left distributes
-- <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
alt_left_distributes
  :: forall a b.
     ( Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b, Eq b
     )
  => Gen a
  -> Gen b
  -> Property
alt_left_distributes genA genB = property $ do
  f <- Fn.forAllFn $ Fn.fn genA

  a <- forAll (genShowDecoder genB)
  b <- forAll (genShowDecoder genB)

  runSD ( f <$> (a <!> b) ) === runSD ( (f <$> a) <!> (f <$> b) )

-- |
-- identity
--
--     pure id <*> v = v
applicative_id :: Property
applicative_id = property $ do
  a <- forAll (genShowDecoder Gen.bool)
  runSD (pure id <*> a) === runSD a

-- |
-- composition
--
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
applicative_composition
  :: forall a b c.
     ( Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b, Eq b
     , Show c, Arg c, Vary c
     )
  => Gen a
  -> Gen b
  -> Gen c
  -> Property
applicative_composition genA genB genC = property $ do
  u <- Fn.forAllFn $ Fn.fn genB
  v <- Fn.forAllFn $ Fn.fn genC

  w <- forAll (genShowDecoder genA)

  let
    dU = pure u
    dV = pure v

  runSD ( pure (.) <*> dU <*> dV <*> w ) === runSD ( dU <*> ( dV <*> w ) )

-- |
-- homomorphism
--
--     pure f <*> pure x = pure (f x)
applicative_homomorphism
  :: forall a b.
     ( Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b
     )
  => Gen a
  -> Gen b
  -> Property
applicative_homomorphism genA genB = property $ do
  f <- Fn.forAllFn $ Fn.fn genA
  x <- forAll genB

  runD (pure f <*> pure x) === runD (pure (f x))

-- |
-- interchange
--
--     u <*> pure y = pure ($ y) <*> u
applicative_interchange
  :: forall u y.
     ( Show u, Arg u, Vary u, Eq u
     , Show y, Arg y, Vary y
     )
  => Gen u
  -> Gen y
  -> Property
applicative_interchange genU genY = property $ do
  u <- Fn.forAllFn $ Fn.fn genU
  y <- forAll genY

  let
    dU = pure u

  runD (dU <*> pure y) === runD (pure ($ y) <*> dU)

decoderLaws :: TestTree
decoderLaws = testGroup "Decoder Laws"
  [ testProperty "Applicative 'identity'" applicative_id
  , testProperty "Applicative 'composition'" $ applicative_composition Gen.bool Gen.bool Gen.bool
  , testProperty "Applicative 'homomorphism'" $ applicative_homomorphism Gen.bool Gen.bool
  , testProperty "Applicative 'interchange'" $ applicative_interchange Gen.bool Gen.bool
  , testProperty "Alt 'associativity'" alt_associativity
  , testProperty "Alt 'left distributes'" $ alt_left_distributes Gen.bool Gen.bool
  ]
