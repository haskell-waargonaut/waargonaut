{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes #-}
module Waargonaut.Encode.Types
  ( Encoder (..)
  , Encoder'
  , ObjEncoder (..)
  , ObjEncoder'
  , AsEncoder (..)
  , generaliseEncoder'
  , generaliseObjEncoder'
  ) where

import           Control.Monad                        (Monad)
import           Control.Monad.Morph                  (MFunctor (..),
                                                       generalize)

import           Control.Applicative                  (Applicative, liftA2,
                                                       pure)
import           Control.Category                     ((.))
import           Control.Lens                         (( # ))

import           Data.Either                          (either)
import           Data.Function                        (const, ($))
import           Data.Functor                         ((<$>))
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
newtype Encoder f a = Encoder
  { runEncoder :: a -> f Json -- ^ Run this 'Encoder' to convert the 'a' to 'Json'
  }

instance Contravariant (Encoder f) where
  contramap f (Encoder g) = Encoder (g . f)

instance MFunctor Encoder where
  hoist nat (Encoder eFn) = Encoder (nat . eFn)

-- | Generalise an 'Encoder' a' to 'Encoder f a'
generaliseEncoder' :: Monad f => Encoder' a -> Encoder f a
generaliseEncoder' = Encoder . fmap generalize . runEncoder
{-# INLINE generaliseEncoder' #-}

-- |
-- As a convenience, this type is a pure Encoder over 'Identity' in place of the @f@.
type Encoder' = Encoder Identity

-- |
newtype ObjEncoder f a = ObjEncoder
  { runObjEncoder :: a -> f (JObject WS Json)
  }

-- |
-- As a convenience, this type is a pure Encoder over 'Identity' in place of the @f@.
type ObjEncoder' = ObjEncoder Identity

instance Contravariant (ObjEncoder f) where
  contramap f (ObjEncoder g) = ObjEncoder (g . f)
  {-# INLINE contramap #-}

instance Applicative f => Divisible (ObjEncoder f) where
  conquer = ObjEncoder (const (pure mempty))
  {-# INLINE conquer #-}

  divide atobc objB objC = ObjEncoder $ \a ->
    let
      (b,c) = atobc a
    in
      liftA2 (<>) (runObjEncoder objB b) (runObjEncoder objC c)
  {-# INLINE divide #-}

instance Applicative f => Decidable (ObjEncoder f) where
  lose f = ObjEncoder $ \a -> absurd (f a)
  {-# INLINE lose #-}

  choose split objB objC = ObjEncoder $ \a ->
    either (runObjEncoder objB) (runObjEncoder objC) (split a)
  {-# INLINE choose #-}

instance MFunctor ObjEncoder where
  hoist nat (ObjEncoder eFn) = ObjEncoder (nat . eFn)
  {-# INLINE hoist #-}

-- | Generalise an 'ObjEncoder' a' to 'ObjEncoder f a'
generaliseObjEncoder' :: Monad f => ObjEncoder' a -> ObjEncoder f a
generaliseObjEncoder' = ObjEncoder . fmap generalize . runObjEncoder
{-# INLINE generaliseObjEncoder' #-}

class AsEncoder e f where
  runToJson :: forall a. e f a -> a -> f Json

instance Applicative f => AsEncoder Encoder f where
  runToJson = runEncoder
  {-# INLINE runToJson #-}

instance Applicative f => AsEncoder ObjEncoder f where
  runToJson (ObjEncoder enc) a = (\o' -> _JObj # (o', mempty)) <$> enc a
  {-# INLINE runToJson #-}
