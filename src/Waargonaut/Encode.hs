{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions to encode your data types to 'Json'.
module Waargonaut.Encode
  (
    -- * Encoder type
    Encoder' (..)
  , Encoder (..)

    -- * Creation
  , encodeA
  , encodeIdentityA
  , runPureEncoder

    -- * Premade Encoders
  , encodeInt
  , encodeBool
  , encodeText
  , encodeArray
  , encodeMapToObj

    -- * Object encoder helpers
  , encodeAsMapLikeObj
  , atKey
  , intAt
  , textAt
  , boolAt
  , arrayAt
  ) where

import           Prelude                    hiding ((.))

import           Control.Category           ((.))
import           Control.Lens               (At, Index, IxValue, Rewrapped,
                                             Wrapped (..), at, cons, iso, ( # ),
                                             (?~), _Empty, _Wrapped)

import           Data.Traversable           (traverse)

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Identity      (Identity (..))

import           Data.Monoid                (mempty)
import           Data.Semigroup             (Semigroup)

import qualified Data.ByteString.Builder    as BB
import           Data.ByteString.Lazy       (ByteString)

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

import           Data.Text                  (Text)

import           Data.Digit                 (HeXDigit)

import           Waargonaut                 (waargonautBuilder)
import           Waargonaut.Types           (AsJType (..), Json,
                                             MapLikeObj (..), WS, textToJString,
                                             wsRemover, _JNumberInt)

-- |
-- Define an "encoder" as a function from some @a@ to some 'Json' with the
-- allowance for some context @f@.
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

-- | Create an 'Encoder'' for 'a' by providing a function from 'a -> f Json'.
encodeA :: (a -> f Json) -> Encoder' f a
encodeA = Encoder'

-- | As 'encodeA' but specialised to 'Identity' when the additional flexibility
-- isn't needed.
encodeIdentityA :: (a -> Json) -> Encoder a
encodeIdentityA f = Encoder $ encodeA (Identity . f)

-- | Run the given 'Encoder' to produce a lazy 'ByteString'.
runPureEncoder :: Encoder a -> a -> ByteString
runPureEncoder enc = BB.toLazyByteString
  . waargonautBuilder wsRemover
  . runIdentity
  . runEncoder (unEncoder enc)

-- | Encode an 'Int'
encodeInt :: Encoder Int
encodeInt = encodeIdentityA $ \i -> _JNum # (_JNumberInt # i, mempty)

-- | Encode a 'Bool'
encodeBool :: Encoder Bool
encodeBool = encodeIdentityA $ \b -> _JBool # (b,mempty)

-- | Encode a 'Text'
encodeText :: Encoder Text
encodeText = encodeIdentityA $ \t -> _JStr # (textToJString t, mempty)

-- | Encode some 'a' that is contained with another 't' structure.
encodeWithInner
  :: ( Applicative f
     , Traversable t
     )
  => (t Json -> Json)
  -> Encoder' f a
  -> Encoder' f (t a)
encodeWithInner f g =
  Encoder' $ fmap f . traverse (runEncoder g)

-- | Encode some 'Traversable' of 'a' into a JSON array.
encodeArray
  :: ( Applicative f
     , Traversable t
     )
  => Encoder' f a
  -> Encoder' f (t a)
encodeArray = encodeWithInner
  (\xs -> _JArr # (_Wrapped # foldr cons mempty xs, mempty))

-- | Encode a 'Map' in a JSON object.
encodeMapToObj
  :: Applicative f
  => Encoder' f a
  -> (k -> Text)
  -> Encoder' f (Map k a)
encodeMapToObj encodeVal kToText =
  let
    mapToCS = Map.foldrWithKey (\k v -> at (kToText k) ?~ v) (_Empty # ())
  in
    encodeWithInner (\xs -> _JObj # (fromMapLikeObj $ mapToCS xs, mempty)) encodeVal

-- | Encode an 'a' at the given index on the JSON object.
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

-- | Encode an 'Int' at the given 'Text' key.
intAt
  :: Text
  -> Int
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
intAt =
  atKey encodeInt

-- | Encode a 'Text' value at the given 'Text' key.
textAt
  :: Text
  -> Text
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
textAt =
  atKey encodeText

-- | Encode a 'Bool' at the given 'Text' key.
boolAt
  :: Text
  -> Bool
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
boolAt =
  atKey encodeBool

-- | Encode a 'Foldable' of 'a' at the given index on a JSON object.
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

-- | Apply a function to update a 'MapLikeObj' and encode that as a JSON object.
--
-- For example, given the following data type:
--
-- @
-- data Image = Image
--   { _imageW        :: Int
--   , _imageH        :: Int
--   , _imageTitle    :: Text
--   , _imageAnimated :: Bool
--   , _imageIDs      :: [Int]
--   }
-- @
--
-- We can use this function to create an encoder, composing the individual
-- update functions to set the keys and values as desired.
--
-- @
-- encodeImage :: Encoder Image
-- encodeImage = encodeAsMapLikeObj $ \\img ->
--   intAt \"Width\" (_imageW img) .           -- ^ Set an 'Int' value at the \"Width\" key.
--   intAt \"Height\" (_imageH img) .
--   textAt \"Title\" (_imageTitle img) .
--   boolAt \"Animated\" (_imageAnimated img) .
--   arrayAt encodeInt \"IDs\" (_imageIDs img) -- ^ Set an @[Int]@ value at the \"IDs\" key.
-- @
--
encodeAsMapLikeObj
  :: ( AsJType Json HeXDigit ws a
     , Semigroup ws
     , Monoid ws
     )
  => (i -> MapLikeObj ws a -> MapLikeObj ws a)
  -> Encoder i
encodeAsMapLikeObj f = encodeIdentityA $ \a ->
  _JObj # (fromMapLikeObj $ f a (_Empty # ()), mempty)
