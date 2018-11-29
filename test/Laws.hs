{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Laws
  ( fmap_compose

  , alt_left_distributes
  , alt_associativity

  , applicative_id
  , applicative_composition
  , applicative_homomorphism
  , applicative_interchange

  , monad_return_bind
  , monad_bind_return_id
  , monad_associativity

  , contravariant_identity
  , contravariant_composition
  , contravariant_identity_with_run
  , contravariant_composition_with_run
  ) where

import           Control.Applicative        (liftA3)

import           Data.Functor.Alt           (Alt (..))
import           Data.Functor.Contravariant (Contravariant, contramap)

import           Hedgehog
import           Hedgehog.Function          (Arg, Vary)
import qualified Hedgehog.Function          as Fn

fmap_compose
  :: forall f a b c
   . ( Functor f
     , Show (f a)
     , Show a, Arg a, Vary a
     , Show b, Arg b, Vary b
     , Show c
     , Eq (f c)
     , Show (f c)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
fmap_compose genF genA genB genC = property $ do
  g <- Fn.forAllFn $ Fn.fn genB
  f <- Fn.forAllFn $ Fn.fn genC
  xs <- forAll $ genF genA
  fmap (f . g) xs === fmap f (fmap g xs)

-- |
-- Alt left distributes
-- <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
alt_left_distributes
  :: forall a b f.
     ( Alt f
     , Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b, Eq b
     , Show (f a), Eq (f a)
     , Show (f b)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Property
alt_left_distributes genF genA genB = property $ do
  f <- Fn.forAllFn $ Fn.fn genA

  a <- forAll (genF genB)
  b <- forAll (genF genB)

  (f <$> (a <!> b)) === ((f <$> a) <!> (f <$> b))

-- |
-- Alt Associative
-- <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
--
alt_associativity
  :: forall f a.
     ( Alt f
     , Show (f a), Eq (f a)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Property
alt_associativity genF genA = property $ do
  (a,b,c) <- forAll $ liftA3 (,,)
    (genF genA)
    (genF genA)
    (genF genA)

  ((a <!> b) <!> c) === (a <!> (b <!> c))

-- |
-- identity
--
--     pure id <*> v = v
applicative_id
  :: forall f a.
     ( Applicative f
     , Show (f a)
     , Eq (f a)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Property
applicative_id genF genA = property $ do
  a <- forAll (genF genA)
  (pure id <*> a) === a

-- |
-- composition
--
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
applicative_composition
  :: forall f a b c.
     ( Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b, Eq b
     , Show c, Arg c, Vary c
     , Show (f a)
     , Show (f b)
     , Show (f c)
     , Eq (f a)
     , Eq (f b)
     , Eq (f c)
     , Applicative f
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
applicative_composition genF genA genB genC = property $ do
  u <- Fn.forAllFn $ Fn.fn genB
  v <- Fn.forAllFn $ Fn.fn genC

  w <- forAll (genF genA)

  let
    dU = pure u
    dV = pure v

  ( pure (.) <*> dU <*> dV <*> w ) === ( dU <*> ( dV <*> w ) )

-- |
-- homomorphism
--
--     pure f <*> pure x = pure (f x)
applicative_homomorphism
  :: forall f a b.
     ( Show a, Arg a, Vary a, Eq a
     , Show b, Arg b, Vary b
     , Show (f a), Eq (f a)
     , Eq (f b)
     , Applicative f
     )
  => (forall x. x -> f x)
  -> Gen a
  -> Gen b
  -> Property
applicative_homomorphism pureF genA genB = property $ do
  f <- Fn.forAllFn $ Fn.fn genA
  x <- forAll genB

  (pureF f <*> pureF x) === (pureF (f x))

-- |
-- interchange
--
--     u <*> pure y = pure ($ y) <*> u
applicative_interchange
  :: forall f u y.
     ( Applicative f
     , Show u, Arg u, Vary u, Eq u
     , Show y, Arg y, Vary y
     , Show (f u), Eq (f u)
     , Show (f y), Eq (f y)
     )
  => (forall x. x -> f x)
  -> Gen u
  -> Gen y
  -> Property
applicative_interchange pureF genU genY = property $ do
  u <- Fn.forAllFn $ Fn.fn genU
  y <- forAll genY

  let
    dU = pureF u

  (dU <*> pure y) === (pure ($ y) <*> dU)

-- |
-- monad
--
--    return a >>= k = k a
monad_return_bind
  :: forall f a k.
     ( Monad f
     , Show a, Arg a, Vary a, Eq a
     , Show k, Arg k, Vary k, Eq k
     , Show (f a), Eq (f a)
     , Show (f k), Eq (f k)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen k
  -> Property
monad_return_bind genF genA genK = property $ do
  k <- Fn.forAllFn $ Fn.fn (genF genK)
  a <- forAll genA

  (return a >>= k) === (k a)

-- |
-- monad
--
--     m >>= return  =  m
monad_bind_return_id
  :: forall f a.
     ( Monad f
     , Show a, Eq a
     , Show (f a), Eq (f a)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Property
monad_bind_return_id genF genA = property $ do
  m <- forAll (genF genA)

  (m >>= return) === m

-- |
-- monad
--
--     m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
monad_associativity
  :: forall f m k h.
     ( Monad f
     , Show m, Arg m, Vary m, Eq m
     , Show k, Arg k, Vary k, Eq k
     , Show h, Arg h, Vary h, Eq h
     , Show (f m), Eq (f m)
     , Show (f k), Eq (f k)
     , Show (f h), Eq (f h)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen m
  -> Gen k
  -> Gen h
  -> Property
monad_associativity genF genM genK genH = property $ do
  m <- forAll (genF genM)

  k <- Fn.forAllFn $ Fn.fn (genF genK)
  h <- Fn.forAllFn $ Fn.fn (genF genH)

  (m >>= (\x -> k x >>= h)) === ( (m >>= k) >>= h )

-- |
-- contravariant
--
--     contramap f . contramap g = contramap (g . f)
contravariant_composition
  :: forall f a b c.
     ( Contravariant f
     , Show a, Arg a, Vary a, Eq (f a), Show (f a)
     , Show b, Arg b, Vary b, Eq b
     , Show c, Show (f c)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
contravariant_composition genF _genA genB genC = property $ do
  f <- Fn.forAllFn $ (Fn.fn genB :: Gen (Fn.Fn a b))
  g <- Fn.forAllFn $ (Fn.fn genC :: Gen (Fn.Fn b c))

  fc <- forAll (genF genC)

  (contramap f . contramap g) fc === contramap (g . f) fc

-- |
-- contravariant
--
--     contramap id a = a
contravariant_identity
  :: forall f a.
     ( Contravariant f
     , Show a, Arg a, Vary a
     , Show (f a)
     , Eq (f a)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Property
contravariant_identity genF genA = property $ do
  a <- forAll (genF genA)

  contramap id a === a

-- |
-- contravariant
--
--     contramap f . contramap g = contramap (g . f)
contravariant_composition_with_run
  :: forall f a b c x.
     ( Contravariant f
     , Show a, Arg a, Vary a
     , Show b, Arg b, Vary b, Eq b
     , Show c, Show (f c)
     , Eq x, Show x
     )
  => (Gen c -> Gen (f c))
  -> (f a -> a -> x)
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
contravariant_composition_with_run genF runF genA genB genC = property $ do
  f <- Fn.forAllFn $ (Fn.fn genB :: Gen (Fn.Fn a b))
  g <- Fn.forAllFn $ (Fn.fn genC :: Gen (Fn.Fn b c))

  a <- forAll genA
  fc <- forAll (genF genC)

  runF ((contramap f . contramap g) fc) a === runF (contramap (g . f) fc) a

-- |
-- contravariant
--
--     contramap id a = a
contravariant_identity_with_run
  :: forall f a b.
     ( Contravariant f
     , Show a, Arg a, Vary a, Eq a
     , Show b, Eq b
     , Show (f a)
     )
  => (Gen a -> Gen (f a))
  -> (f a -> a -> b)
  -> Gen a
  -> Property
contravariant_identity_with_run genF runF genA = property $ do
  fa <- forAll (genF genA)
  a <- forAll genA

  runF (contramap id fa) a === runF fa a
