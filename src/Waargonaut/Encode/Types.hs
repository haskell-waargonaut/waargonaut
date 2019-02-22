{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Types and functions that make up the internal structure of the encoders.
--
module Waargonaut.Encode.Types
  ( -- * Types
    EncoderFns (..)

    -- * Useful aliases
  , Encoder
  , Encoder'
  , ObjEncoder
  , ObjEncoder'

    -- * Runners
  , runEncoder
  , runPureEncoder

    -- * Helpers
  , jsonEncoder
  , objEncoder
  , generaliseEncoder
  ) where

import           Control.Monad                        (Monad)
import           Control.Monad.Morph                  (MFunctor (..),
                                                       generalize)

import           Control.Applicative                  (Applicative, liftA2,
                                                       pure)
import           Control.Category                     (id, (.))
import           Control.Lens                         (( # ))

import           Data.Either                          (either)
import           Data.Function                        (const, ($))
import           Data.Functor                         (Functor)
import           Data.Functor.Contravariant           (Contravariant (..))

import           Data.Functor.Contravariant.Divisible (Decidable (..),
                                                       Divisible (..))
import           Data.Monoid                          (mempty)
import           Data.Semigroup                       ((<>))
import           Data.Void                            (absurd)

import           Data.Functor                         (fmap)
import           Data.Functor.Identity                (Identity (..))

import           Waargonaut.Types                     (JObject, Json, WS, _JObj)


-- |
-- Define an "encoder" as a function from some @a@ to some 'Json' with the
-- allowance for some context @f@.
--
-- The helper functions 'jsonEncoder' and 'objEncoder' are probably what you
-- want to use.
--
data EncoderFns i f a = EncoderFns
  { finaliseEncoding :: i -> Json -- ^ The @i@ need not be the final 'Json' structure. This function will complete the output from 'initialEncoding' to the final 'Json' output.

  , initialEncoding  :: a -> f i -- ^ Run the initial encoding step of the given input. This lets you encode the @a@ to an intermediate structure before utilising the 'finaliseEncoding' function to complete the process.
  }

instance MFunctor (EncoderFns i) where
  hoist nat (EncoderFns f i) = EncoderFns f (nat . i)

-- | Generalise any 'Encoder' a' to 'Encoder f a'
generaliseEncoder :: Monad f => EncoderFns i Identity a -> EncoderFns i f a
generaliseEncoder (EncoderFns f i) = EncoderFns f (generalize . i)

instance Contravariant (EncoderFns o f) where
  contramap f e = EncoderFns (finaliseEncoding e) (initialEncoding e . f)
  {-# INLINE contramap #-}

instance Applicative f => Divisible (EncoderFns (JObject WS Json) f) where
  conquer = objEncoder (const (pure mempty))
  {-# INLINE conquer #-}

  divide atobc (EncoderFns _ oB) (EncoderFns _ oC) = objEncoder $ \a ->
    let
      (b,c) = atobc a
    in
      liftA2 (<>) (oB b) (oC c)
  {-# INLINE divide #-}

instance Applicative f => Decidable (EncoderFns (JObject WS Json) f) where
  lose f = objEncoder $ \a -> absurd (f a)
  {-# INLINE lose #-}

  choose split (EncoderFns _ oB) (EncoderFns _ oC) = objEncoder $ \a ->
    either oB oC (split a)
  {-# INLINE choose #-}

-- | As a convenience, this type defines the @i@ to be a specific 'Json' structure:
type Encoder f a = EncoderFns Json f a

-- | As a convenience, this type defines the @i@ to be a specific 'JObject WS Json' structure:
type ObjEncoder f a = EncoderFns (JObject WS Json) f a

-- | As a convenience, this type is a pure Encoder over 'Identity' in place of the @f@.
type Encoder' a = EncoderFns Json Identity a
-- | As a convenience, this type is a pure ObjEncoder over 'Identity' in place of the @f@.
type ObjEncoder' a = EncoderFns (JObject WS Json) Identity a

-- | Run any encoder to the 'Json' representation, allowing for some
-- 'Functor' context @f@.
runEncoder :: Functor f => EncoderFns i f a -> a -> f Json
runEncoder e = fmap (finaliseEncoding e) . initialEncoding e
{-# INLINE runEncoder #-}

-- | Run any encoder to the 'Json' representation, with the context specialised
-- to 'Identity' for convenience.
runPureEncoder :: EncoderFns i Identity a -> a -> Json
runPureEncoder e = runIdentity . fmap (finaliseEncoding e) . initialEncoding e
{-# INLINE runPureEncoder #-}

-- | Helper function for creating an 'Encoder', provides the default
-- 'finaliseEncoding' function for 'Json' encoders.
jsonEncoder :: (a -> f Json) -> EncoderFns Json f a
jsonEncoder = EncoderFns id
{-# INLINE jsonEncoder #-}

-- | Helper function for creating a JSON 'object' 'Encoder'. Provides the
-- default 'finaliseEncoding' function for completing the 'JObject' to the
-- necessary 'Json' type.
objEncoder :: (a -> f (JObject WS Json)) -> EncoderFns (JObject WS Json) f a
objEncoder = EncoderFns (\o -> _JObj # (o, mempty))
{-# INLINE objEncoder #-}
