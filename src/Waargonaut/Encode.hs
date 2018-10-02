{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions to encode your data types to 'Json'.
module Waargonaut.Encode
  (
    -- * Encoder type
    Encoder (..)
  , Encoder'

    -- * Creation
  , encodeA
  , encodePureA
  , runPureEncoder

    -- * Provided encoders
  , int
  , bool
  , text
  , null
  , either
  , maybe
  , maybeOrNull
  , traversable
  , list
  , nonempty
  , mapToObj
  , json

    -- * Object encoder helpers
  , mapLikeObj
  , atKey
  , intAt
  , textAt
  , boolAt
  , traversableAt
  , listAt
  , nonemptyAt
  , encAt
  , keyValuesAsObj

    -- * Encoders specialised to Identity
  , int'
  , bool'
  , text'
  , null'
  , either'
  , maybe'
  , maybeOrNull'
  , traversable'
  , nonempty'
  , list'
  , atKey'
  , mapLikeObj'
  , mapToObj'
  , keyValuesAsObj'
  , json'

  ) where

import           Control.Applicative        (Applicative (..), (<$>))
import           Control.Category           (id, (.))
import           Control.Lens               (AReview, At, Index, IxValue,
                                             Rewrapped, Wrapped (..), at, cons,
                                             iso, ( # ), (?~), _Empty, _Wrapped)
import qualified Control.Lens               as L

import           Prelude                    (Bool, Int, Monad)

import           Data.Foldable              (Foldable, foldr, foldrM)
import           Data.Function              (const, flip, ($), (&))
import           Data.Functor               (Functor, fmap)
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Traversable           (Traversable, traverse)

import           Data.Either                (Either)
import qualified Data.Either                as Either
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Maybe                 (Maybe)
import qualified Data.Maybe                 as Maybe

import           Data.Monoid                (Monoid, mempty)
import           Data.Semigroup             (Semigroup)

import qualified Data.ByteString.Builder    as BB
import           Data.ByteString.Lazy       (ByteString)

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

import           Data.Text                  (Text)

import           Waargonaut                 (waargonautBuilder)
import           Waargonaut.Types           (AsJType (..), JAssoc (..), JObject,
                                             Json, MapLikeObj (..), WS,
                                             textToJString, wsRemover,
                                             _JNumberInt)

-- |
-- Define an "encoder" as a function from some @a@ to some 'Json' with the
-- allowance for some context @f@.
--
newtype Encoder f a = Encoder
  { runEncoder :: a -> f Json
  }

instance (Encoder f a) ~ t => Rewrapped (Encoder f a) t

instance Wrapped (Encoder f a) where
  type Unwrapped (Encoder f a) = a -> f Json
  _Wrapped' = iso runEncoder Encoder

instance Contravariant (Encoder f) where
  contramap f (Encoder g) = Encoder (g . f)

-- |
-- As a convenience, this type is a pure Encoder over 'Identity' in place of the @f@.
type Encoder' = Encoder Identity

-- | Create an 'Encoder'' for 'a' by providing a function from 'a -> f Json'.
encodeA :: (a -> f Json) -> Encoder f a
encodeA = Encoder

-- | As 'encodeA' but specialised to 'Identity' when the additional flexibility
-- isn't needed.
encodePureA :: (a -> Json) -> Encoder' a
encodePureA f = encodeA (Identity . f)

-- | Run the given 'Encoder' to produce a lazy 'ByteString'.
runPureEncoder :: Encoder' a -> a -> ByteString
runPureEncoder enc = BB.toLazyByteString
  . waargonautBuilder wsRemover
  . runIdentity
  . runEncoder enc

-- | 'Encoder'' for a Waargonaut 'Json' data structure
json :: Applicative f => Encoder f Json
json = encodeA pure

encJ
  :: ( Monoid t
     , Applicative f
     )
  => AReview Json (b, t)
  -> (a -> b)
  -> Encoder f a
encJ c f =
  encodeA (pure . (c #) . (,mempty) . f)

-- | Encode an 'Int'
int :: Applicative f => Encoder f Int
int = encJ _JNum (_JNumberInt #)

-- | Encode a 'Bool'
bool :: Applicative f => Encoder f Bool
bool = encJ _JBool id

-- | Encode a 'Text'
text :: Applicative f => Encoder f Text
text = encJ _JStr textToJString

-- | Encode an explicit 'null'.
null :: Applicative f => Encoder f ()
null = encodeA $ const (pure $ _JNull # mempty)

-- | Encode a 'Maybe' value, using the provided 'Encoder''s to handle the
-- different choices.
maybe
  :: Applicative f
  => Encoder f ()
  -> Encoder f a
  -> Encoder f (Maybe a)
maybe encN = encodeA
  . Maybe.maybe (runEncoder encN ())
  . runEncoder

-- | Encode a 'Maybe a' to either 'Encoder a' or 'null'
maybeOrNull
  :: Applicative f
  => Encoder f a
  -> Encoder f (Maybe a)
maybeOrNull =
  maybe null

-- | Encode an 'Either' value using the given 'Encoder's
either
  :: Applicative f
  => Encoder f a
  -> Encoder f b
  -> Encoder f (Either a b)
either eA = encodeA
  . Either.either (runEncoder eA)
  . runEncoder

-- | Encode some 'Traversable' of 'a' into a JSON array.
traversable
  :: ( Applicative f
     , Traversable t
     )
  => Encoder f a
  -> Encoder f (t a)
traversable = encodeWithInner
  (\xs -> _JArr # (_Wrapped # foldr cons mempty xs, mempty))

-- | Encode a 'Map' in a JSON object.
mapToObj
  :: Applicative f
  => Encoder f a
  -> (k -> Text)
  -> Encoder f (Map k a)
mapToObj encodeVal kToText =
  let
    mapToCS = Map.foldrWithKey (\k v -> at (kToText k) ?~ v) (_Empty # ())
  in
    encodeWithInner (\xs -> _JObj # (fromMapLikeObj $ mapToCS xs, mempty)) encodeVal

-- | Encode a 'NonEmpty' list
nonempty
  :: Applicative f
  => Encoder f a
  -> Encoder f (NonEmpty a)
nonempty =
  traversable

-- | Encode a standard Haskell list
list
  :: Applicative f
  => Encoder f a
  -> Encoder f [a]
list =
  traversable

json' :: Encoder' Json
json' = json

int' :: Encoder' Int
int' = int

bool' :: Encoder' Bool
bool' = bool

text' :: Encoder' Text
text' = text

null' :: Encoder' ()
null' = null

maybe'
  :: Encoder' ()
  -> Encoder' a
  -> Encoder' (Maybe a)
maybe' =
  maybe

maybeOrNull'
  :: Encoder' a
  -> Encoder' (Maybe a)
maybeOrNull' =
  maybeOrNull

either'
  :: Encoder' a
  -> Encoder' b
  -> Encoder' (Either a b)
either' =
  either

nonempty'
  :: Encoder' a
  -> Encoder' (NonEmpty a)
nonempty' =
  traversable

list'
  :: Encoder' a
  -> Encoder' [a]
list' =
  traversable

-- | Encode some 'a' that is contained with another 't' structure.
encodeWithInner
  :: ( Applicative f
     , Traversable t
     )
  => (t Json -> Json)
  -> Encoder f a
  -> Encoder f (t a)
encodeWithInner f g =
  Encoder $ fmap f . traverse (runEncoder g)

traversable'
  :: Traversable t
  => Encoder' a
  -> Encoder' (t a)
traversable' =
  traversable

-- | Using the given function to convert the 'k' type keys to a 'Text' value,
-- encode a 'Map' as a JSON object.
mapToObj'
  :: Encoder' a
  -> (k -> Text)
  -> Encoder' (Map k a)
mapToObj' =
  mapToObj

atKey
  :: ( At t
     , IxValue t ~ Json
     , Applicative f
     )
  => Index t
  -> Encoder f a
  -> a
  -> t
  -> f t
atKey k enc v t =
  (\v' -> t & at k ?~ v') <$> runEncoder enc v

-- | Encode an 'a' at the given index on the JSON object.
atKey'
  :: ( At t
     , IxValue t ~ Json
     )
  => Index t
  -> Encoder' a
  -> a
  -> t
  -> t
atKey' k enc v =
  at k ?~ runIdentity (runEncoder enc v)

-- | Encode an 'Int' at the given 'Text' key.
intAt
  :: Text
  -> Int
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
intAt =
  flip atKey' int

-- | Encode a 'Text' value at the given 'Text' key.
textAt
  :: Text
  -> Text
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
textAt =
  flip atKey' text

-- | Encode a 'Bool' at the given 'Text' key.
boolAt
  :: Text
  -> Bool
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
boolAt =
  flip atKey' bool

-- | Encode a 'Foldable' of 'a' at the given index on a JSON object.
traversableAt
  :: ( At t
     , Traversable f
     , IxValue t ~ Json
     )
  => Encoder' a
  -> Index t
  -> f a
  -> t
  -> t
traversableAt enc =
  flip atKey' (traversable enc)

-- | Encode a standard Haskell list at the given index on a JSON object.
listAt
  :: ( At t
     , IxValue t ~ Json
     )
  => Encoder' a
  -> Index t
  -> [a]
  -> t
  -> t
listAt =
  traversableAt

-- | Encode a 'NonEmpty' list at the given index on a JSON object.
nonemptyAt
  :: ( At t
     , IxValue t ~ Json
     )
  => Encoder' a
  -> Index t
  -> NonEmpty a
  -> t
  -> t
nonemptyAt =
  traversableAt

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
-- encodeImage :: Applicative f => Encoder f Image
-- encodeImage = encodeAsMapLikeObj $ \\img ->
--   intAt \"Width\" (_imageW img) .           -- ^ Set an 'Int' value at the \"Width\" key.
--   intAt \"Height\" (_imageH img) .
--   textAt \"Title\" (_imageTitle img) .
--   boolAt \"Animated\" (_imageAnimated img) .
--   arrayAt int \"IDs\" (_imageIDs img) -- ^ Set an @[Int]@ value at the \"IDs\" key.
-- @
--
mapLikeObj
  :: ( AsJType Json ws a
     , Semigroup ws
     , Monoid ws
     , Applicative f
     )
  => (i -> MapLikeObj ws a -> MapLikeObj ws a)
  -> Encoder f i
mapLikeObj f = encodeA $ \a ->
  pure $ _JObj # (fromMapLikeObj $ f a (_Empty # ()), mempty)

-- | As per 'mapLikeObj' but specialised for 'Identity' as the 'Applicative'.
mapLikeObj'
  :: ( AsJType Json ws a
     , Semigroup ws
     , Monoid ws
     )
  => (i -> MapLikeObj ws a -> MapLikeObj ws a)
  -> Encoder' i
mapLikeObj' f = encodePureA $ \a ->
  _JObj # (fromMapLikeObj $ f a (_Empty # ()), mempty)

onObj
  :: Applicative f
  => Text
  -> b
  -> Encoder f b
  -> JObject WS Json
  -> f (JObject WS Json)
onObj k b encB o = (\j -> o & _Wrapped L.%~ L.cons j)
  . JAssoc (textToJString k) mempty mempty <$> runEncoder encB b

-- | Encode key value pairs as a JSON object, allowing duplicate keys.
keyValuesAsObj
  :: ( Foldable g
     , Semigroup ws
     , Monoid ws
     , AsJType Json ws j
     , Monad f
     )
  => g (a -> JObject WS Json -> f (JObject WS Json))
  -> Encoder f a
keyValuesAsObj xs = encodeA $ \a ->
  (\v -> _JObj # (v,mempty)) <$> foldrM (\f -> f a) (_Empty # ()) xs

-- | As per 'keyValuesAsObj' but with the 'f' specialised to 'Identity'.
keyValuesAsObj'
  :: ( Foldable g
     , Functor g
     , Semigroup ws
     , Monoid ws
     , AsJType Json ws j
     )
  => g (a -> JObject WS Json -> JObject WS Json)
  -> Encoder' a
keyValuesAsObj' =
  keyValuesAsObj . fmap (\f a -> Identity . f a)

-- | Using a given 'Encoder', encode a key value pair on the JSON object, using
-- the accessor function to retrieve the value.
encAt
  :: Applicative f
  => Encoder f b
  -> Text
  -> (a -> b)
  -> a
  -> JObject WS Json
  -> f (JObject WS Json)
encAt e k f a =
  onObj k (f a) e
