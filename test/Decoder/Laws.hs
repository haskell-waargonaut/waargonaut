{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Decoder.Laws (decoderLaws) where

import           Control.Applicative     (Applicative, pure)
import           Control.Monad.Except    (throwError)

import           Data.Functor.Alt        (Alt ((<!>)))
import           Data.Functor.Identity   (Identity)

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.Hedgehog     (testProperty)

import           Hedgehog
import qualified Hedgehog.Gen            as Gen

import qualified Waargonaut.Attoparsec   as WA
import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (DecodeError (ConversionFailure))
import           Waargonaut.Decode.Types (Decoder)

import qualified Laws

runD :: Decoder Identity a -> Either (DecodeError, D.CursorHistory) a
runD d = WA.pureDecodeAttoparsecText d "true"

newtype ShowDecoder a = SD (Decoder Identity a)
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
  , SD        <$> Gen.constant (throwError $ ConversionFailure "Intentional DecodeError (TEST)")
  ]

decoderLaws :: TestTree
decoderLaws = testGroup "Decoder Laws"
  [ testGroup "Applicative"

    [ testProperty "identity"     $ Laws.applicative_id genShowDecoder Gen.bool
    , testProperty "composition"  $ Laws.applicative_composition genShowDecoder Gen.bool Gen.bool Gen.bool
    , testProperty "homomorphism" $ Laws.applicative_homomorphism sdPure Gen.bool Gen.bool
    , testProperty "interchange"  $ Laws.applicative_interchange sdPure Gen.bool Gen.bool
    ]

  , testGroup "Alt"
    [ testProperty "associativity"    $ Laws.alt_associativity genShowDecoder Gen.bool
    , testProperty "left distributes" $ Laws.alt_left_distributes genShowDecoder Gen.bool Gen.bool
    ]

  , testGroup "Monad"
    [ testProperty "return a >>= k = k a" $ Laws.monad_return_bind genShowDecoder Gen.bool Gen.bool
    , testProperty "m >>= return = m"     $ Laws.monad_bind_return_id genShowDecoder Gen.bool
    , testProperty "associativity"        $ Laws.monad_associativity genShowDecoder Gen.bool Gen.bool Gen.bool
    ]

  , testGroup "Functor"
    [ testProperty "'fmap compose'" $ Laws.fmap_compose genShowDecoder Gen.bool Gen.bool Gen.bool
    ]
  ]
  where
    sdPure = (pure :: a -> ShowDecoder a)
