{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Types and functions to encode your data types to 'Json'.
module Waargonaut.Encode
  (
    -- * Encoder type
    Encoder' (..)
  , Encoder (..)

    -- * Creation
  , encodeA
  , encodePureA
  , runPureEncoder

    -- * Pure Encoders
  , int
  , bool
  , text
  , null
  , either
  , maybe
  , maybeOrNull
  , array
  , mapToObj

    -- * Object encoder helpers
  , mapLikeObj
  , atKey
  , intAt
  , textAt
  , boolAt
  , arrayAt

    -- * Applicative Encoders
  , int'
  , bool'
  , text'
  , null'
  , either'
  , maybe'
  , maybeOrNull'
  , array'
  , mapToObj'

  ) where

import           Control.Applicative        (Applicative (..))
import           Control.Category           ((.),id)
import           Control.Lens               (At, Index, IxValue, Rewrapped, AReview,
                                             Wrapped (..), at, cons, iso, ( # ),
                                             (?~), _Empty, _Wrapped)
import           Prelude                    (Bool, Int)

import           Data.Foldable              (Foldable, foldr)
import           Data.Function              (const, ($))
import           Data.Functor               (fmap)
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Traversable           (Traversable, traverse)

import           Data.Either                (Either)
import qualified Data.Either                as Either
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
  deriving (Contravariant)

instance (Encoder a) ~ t => Rewrapped (Encoder a) t

instance Wrapped (Encoder a) where
  type Unwrapped (Encoder a) = Encoder' Identity a
  _Wrapped' = iso unEncoder Encoder

-- | Create an 'Encoder'' for 'a' by providing a function from 'a -> f Json'.
encodeA :: (a -> f Json) -> Encoder' f a
encodeA = Encoder'

-- | As 'encodeA' but specialised to 'Identity' when the additional flexibility
-- isn't needed.
encodePureA :: (a -> Json) -> Encoder a
encodePureA f = Encoder $ encodeA (Identity . f)

-- | Run the given 'Encoder' to produce a lazy 'ByteString'.
runPureEncoder :: Encoder a -> a -> ByteString
runPureEncoder enc = BB.toLazyByteString
  . waargonautBuilder wsRemover
  . runIdentity
  . runEncoder (unEncoder enc)

encJ
  :: ( Monoid t
     , Applicative f
     )
  => AReview Json (b, t)
  -> (a -> b)
  -> Encoder' f a
encJ c f =
  encodeA (pure . (c #) . (,mempty) . f)

-- | Encode an 'Int'
int' :: Applicative f => Encoder' f Int
int' = encJ _JNum (_JNumberInt #)

-- | Encode a 'Bool'
bool' :: Applicative f => Encoder' f Bool
bool' = encJ _JBool id

-- | Encode a 'Text'
text' :: Applicative f => Encoder' f Text
text' = encJ _JStr textToJString

null' :: Applicative f => Encoder' f ()
null' = encodeA $ const (pure $ _JNull # mempty)

maybe'
  :: Applicative f
  => Encoder' f ()
  -> Encoder' f a
  -> Encoder' f (Maybe a)
maybe' encN = encodeA
  . Maybe.maybe (runEncoder encN ())
  . runEncoder

-- | Encode a 'Maybe a' to either 'Encoder a' or 'null'
maybeOrNull'
  :: Applicative f
  => Encoder' f a
  -> Encoder' f (Maybe a)
maybeOrNull' =
  maybe' null'

either'
  :: Applicative f
  => Encoder' f a
  -> Encoder' f b
  -> Encoder' f (Either a b)
either' eA = encodeA
  . Either.either (runEncoder eA)
  . runEncoder

int :: Encoder Int
int = Encoder int'

bool :: Encoder Bool
bool = Encoder bool'

text :: Encoder Text
text = Encoder text'

null :: Encoder ()
null = Encoder null'

maybe
  :: Encoder ()
  -> Encoder a
  -> Encoder (Maybe a)
maybe a = Encoder
  . maybe' (unEncoder a)
  . unEncoder

maybeOrNull
  :: Encoder a
  -> Encoder (Maybe a)
maybeOrNull =
  maybe null

either
  :: Encoder a
  -> Encoder b
  -> Encoder (Either a b)
either a = Encoder
  . either' (unEncoder a)
  . unEncoder

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
array'
  :: ( Applicative f
     , Traversable t
     )
  => Encoder' f a
  -> Encoder' f (t a)
array' = encodeWithInner
  (\xs -> _JArr # (_Wrapped # foldr cons mempty xs, mempty))

array
  :: Traversable t
  => Encoder a
  -> Encoder (t a)
array = Encoder
  . array'
  . unEncoder

-- | Encode a 'Map' in a JSON object.
mapToObj'
  :: Applicative f
  => Encoder' f a
  -> (k -> Text)
  -> Encoder' f (Map k a)
mapToObj' encodeVal kToText =
  let
    mapToCS = Map.foldrWithKey (\k v -> at (kToText k) ?~ v) (_Empty # ())
  in
    encodeWithInner (\xs -> _JObj # (fromMapLikeObj $ mapToCS xs, mempty)) encodeVal

mapToObj
  :: Encoder a
  -> (k -> Text)
  -> Encoder (Map k a)
mapToObj e = Encoder
  . mapToObj' (unEncoder e)

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
  atKey int

-- | Encode a 'Text' value at the given 'Text' key.
textAt
  :: Text
  -> Text
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
textAt =
  atKey text

-- | Encode a 'Bool' at the given 'Text' key.
boolAt
  :: Text
  -> Bool
  -> MapLikeObj WS Json
  -> MapLikeObj WS Json
boolAt =
  atKey bool

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
  atKey (array enc)

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
-- encodeImage :: Applicative f => Encoder' f Image
-- encodeImage = encodeAsMapLikeObj $ \\img ->
--   intAt \"Width\" (_imageW img) .           -- ^ Set an 'Int' value at the \"Width\" key.
--   intAt \"Height\" (_imageH img) .
--   textAt \"Title\" (_imageTitle img) .
--   boolAt \"Animated\" (_imageAnimated img) .
--   arrayAt encodeInt \"IDs\" (_imageIDs img) -- ^ Set an @[Int]@ value at the \"IDs\" key.
-- @
--
mapLikeObj
  :: ( AsJType Json ws a
     , Semigroup ws
     , Monoid ws
     )
  => (i -> MapLikeObj ws a -> MapLikeObj ws a)
  -> Encoder i
mapLikeObj f = encodePureA $ \a ->
  _JObj # (fromMapLikeObj $ f a (_Empty # ()), mempty)
