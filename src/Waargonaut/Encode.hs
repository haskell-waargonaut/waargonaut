{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Encode where

import           Prelude                    hiding ((.))

import           Control.Category           ((.))
import           Control.Lens               (At, Index, IxValue, Rewrapped,
                                             Wrapped (..), at, cons, iso, ( # ),
                                             (?~), _Wrapped)

import           Data.Traversable           (traverse)

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Identity      (Identity (..))

import           Data.Monoid                (mempty)
import           Data.Semigroup             (Semigroup)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BB

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

import           Data.Text                  (Text)

import qualified Data.Digit                 as D

import Waargonaut (waargonautBuilder)
import           Waargonaut.Types           (AsJTypes (..), Json,
                                             MapLikeObj (..), WS, emptyMapLike, wsRemover,
                                             _JNumberInt, _JStringText)

-- |
-- Define an "encoder" as a function from some @a@ to some 'Json' with the
-- allowance for a given (but unknown) context @f@.
--
newtype Encoder' f a = Encoder'
  { runEncoder :: a -> f Json
  }

instance (Encoder' f a) ~ t => Rewrapped (Encoder' f a) t

instance Wrapped (Encoder' f a) where
  type Unwrapped (Encoder' f a) = a -> f Json
  _Wrapped' = iso runEncoder Encoder'

instance Contravariant (Encoder' f) where
  contramap f (Encoder' g) = Encoder' (g . f)

-- |
-- As a convenience, this type is a pure Encoder over 'Identity' in place of the @f@.
newtype Encoder a = Encoder
  { unEncoder :: Encoder' Identity a
  }

instance (Encoder a) ~ t => Rewrapped (Encoder a) t

instance Wrapped (Encoder a) where
  type Unwrapped (Encoder a) = Encoder' Identity a
  _Wrapped' = iso unEncoder Encoder

encodeA :: (a -> f Json) -> Encoder' f a
encodeA = Encoder'

encodeIdentityA :: (a -> Json) -> Encoder a
encodeIdentityA f = Encoder $ encodeA (Identity . f)

runPureEncoder
  :: Encoder a
  -> a
  -> ByteString
runPureEncoder enc = BB.toLazyByteString
  . waargonautBuilder wsRemover
  . runIdentity
  . runEncoder (unEncoder enc)

encodeInt
  :: Encoder Int
encodeInt = encodeIdentityA $ \i ->
  _JNum # (_JNumberInt # i, mempty)

encodeBool
  :: Encoder Bool
encodeBool = encodeIdentityA $ \b ->
  _JBool # (b,mempty)

encodeWithInner
  :: ( Applicative f
     , Traversable t
     )
  => (t Json -> Json)
  -> Encoder' f a
  -> Encoder' f (t a)
encodeWithInner f g =
  Encoder' $ fmap f . traverse (runEncoder g)

encodeArray
  :: ( Applicative f
     , Traversable t
     )
  => Encoder' f a
  -> Encoder' f (t a)
encodeArray = encodeWithInner
  (\xs -> _JArr # (_Wrapped # foldr cons mempty xs, mempty))

encodeMapToObj
  :: Applicative f
  => Encoder' f a
  -> (k -> Text)
  -> Encoder' f (Map k a)
encodeMapToObj encodeVal kToText =
  let
    mapToCS = Map.foldrWithKey (\k v -> at (kToText k) ?~ v) emptyMapLike
  in
    encodeWithInner (\xs -> _JObj # (fromMapLikeObj $ mapToCS xs, mempty)) encodeVal

encodeText
  :: Encoder Text
encodeText = encodeIdentityA $ \t ->
  _JStr # (_JStringText # t, mempty)

atKey
  :: ( At t
     , IxValue t ~ Json
     )
  => Encoder a
  -> Index t
  -> a
  -> t
  -> t
atKey enc k v =
  at k ?~ runIdentity (runEncoder (unEncoder enc) v)

intAt
  :: Text
  -> Int
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
intAt =
  atKey encodeInt

textAt
  :: Text
  -> Text
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
textAt =
  atKey encodeText

boolAt
  :: Text
  -> Bool
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
boolAt =
  atKey encodeBool

arrayAt
  :: ( At t
     , Traversable f
     , Foldable f
     , IxValue t ~ Json
     )
  => Encoder a
  -> Index t
  -> f a
  -> t
  -> t
arrayAt enc =
  atKey (Encoder $ encodeArray (unEncoder enc))

encodeAsMapLikeObj
  :: ( AsJTypes Json D.Digit ws a
     , Semigroup ws
     , Monoid ws
     )
  => (i -> MapLikeObj ws a -> MapLikeObj ws a)
  -> Encoder i
encodeAsMapLikeObj f = encodeIdentityA $ \a ->
  _JObj # (fromMapLikeObj $ f a emptyMapLike, mempty)
