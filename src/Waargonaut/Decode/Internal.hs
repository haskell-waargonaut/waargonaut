{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Internal types and functions for building Decoder infrastructure.
module Waargonaut.Decode.Internal
  ( CursorHistory' (..)
  , ppCursorHistory
  , DecodeResultT (..)
  , Decoder' (..)

  , withCursor'
  , runDecoderResultT
  , try
  , recordZipperMove

    -- * Generalised Decoder Functions
  , null'
  , int'
  , text'
  , string'
  , unboundedChar'
  , boundedChar'
  , bool'
  , array'
  , integral'
  , scientific'
  , objTuples'
  , foldCursor'
  , prismDOrFail'

    -- * JSON Object to Map Functions
  , mapKeepingF
  , mapKeepingFirst
  , mapKeepingLast

    -- * Re-exports
  , module Waargonaut.Decode.Error
  , module Waargonaut.Decode.ZipperMove
  ) where

import           Control.Applicative           (liftA2, (<|>))
import           Control.Lens                  (Rewrapped, Wrapped (..), (%=),
                                                _1, _Wrapped)
import qualified Control.Lens                  as L

import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (ExceptT (..), MonadError (..),
                                                liftEither, runExceptT)
import           Control.Monad.State           (MonadState (..), StateT (..))
import           Control.Monad.Trans.Class     (MonadTrans (lift))

import           Control.Monad.Error.Hoist     ((<!?>))
import           Control.Monad.Morph           (MFunctor (..), MMonad (..))

import           Data.Bifunctor                (first)
import           Data.Functor                  (($>))
import           Data.Sequence                 (Seq)

import           Data.Text                     (Text)

import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import qualified Data.Vector                   as V

import qualified Data.Witherable               as Wither

import           Data.Scientific               (Scientific)
import qualified Data.Scientific               as Sci

import           Waargonaut.Types              (AsJType (..), JString,
                                                jNumberToScientific,
                                                jsonAssocKey, jsonAssocVal,
                                                _JChar, _JString)
import           Waargonaut.Types.CommaSep     (toList)
import           Waargonaut.Types.JChar        (jCharToUtf8Char)

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))

import           Waargonaut.Decode.Error       (AsDecodeError (..),
                                                DecodeError (..))
import           Waargonaut.Decode.ZipperMove  (ZipperMove (..), ppZipperMove)


-- |
-- Track the history of the cursor as we move around the zipper.
--
-- It is indexed over the type of the index used to navigate the zipper.
newtype CursorHistory' i = CursorHistory'
  { unCursorHistory' :: Seq (ZipperMove, i)
  }
  deriving (Show, Eq)

-- |
-- Pretty print the given 'CursorHistory'' to a more useful format compared to a 'Seq' of 'i'.
ppCursorHistory
  :: CursorHistory' i
  -> Doc a
ppCursorHistory =
  foldr (<+>) mempty
  . fmap (ppZipperMove . fst)
  . unCursorHistory'

instance CursorHistory' i ~ t => Rewrapped (CursorHistory' i) t

instance Wrapped (CursorHistory' i) where
  type Unwrapped (CursorHistory' i) = Seq (ZipperMove, i)
  _Wrapped' = L.iso (\(CursorHistory' x) -> x) CursorHistory'
  {-# INLINE _Wrapped' #-}

-- |
-- The general structure used to maintain the history of the moves around the
-- zipper, as well as handle the decoding or movement errors that may occur.
-- This structure is generalised of the inner @f@ to allow you to interleave the
-- decoding with your own actions. As well as the error type @e@ so that you may
-- provide your own error type.
--
-- If you use the provided `Waargonaut.Decode` module then you probably won't
-- need to care about this type. It is provided so that you're not limited to
-- how we decide you should be running your decoder.
--
newtype DecodeResultT i e f a = DecodeResultT
  { runDecodeResult :: ExceptT e (StateT (CursorHistory' i) f) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (CursorHistory' i)
           , MonadError e
           )

instance MonadTrans (DecodeResultT i e) where
  lift = DecodeResultT . lift . lift

instance MFunctor (DecodeResultT i e) where
  hoist nat (DecodeResultT dr) = DecodeResultT (hoist (hoist nat) dr)

instance MMonad (DecodeResultT i e) where
  embed t dr = DecodeResultT $ do
    (e, hist) <- runDecodeResult (t (runner dr))
    put hist
    liftEither e
      where
        runner = flip runStateT (CursorHistory' mempty)
          . runExceptT . runDecodeResult

-- |
-- Wrapper type to describe a "Decoder" from something that has a 'Json'ish
-- value @c@, to some representation of @a@.
--
newtype Decoder' c i e f a = Decoder'
  { runDecoder' :: c -> DecodeResultT i e f a
  }
  deriving Functor

instance Monad f => Applicative (Decoder' c i e f) where
  pure       = pure
  aToB <*> a = Decoder' $ \c -> runDecoder' aToB c <*> runDecoder' a c

instance Monad f => Monad (Decoder' c i e f) where
  return      = pure
  a >>= aToFb = Decoder' $ \c -> runDecoder' a c >>= ($ c) . runDecoder' . aToFb

instance MonadTrans (Decoder' c i e) where
  lift = Decoder' . const . lift

instance MFunctor (Decoder' c i e) where
  hoist nat (Decoder' f) = Decoder' (hoist nat . f)

-- |
-- Helper function for constructing a 'Decoder''.
--
-- This function is used by the implemented decoders to simplify constructing a
-- more specific 'Decoder' type.
--
-- @
-- withCursor' $ \curs ->
--   ...
--   ...
-- @
--
withCursor'
  :: (c -> DecodeResultT i e f a)
  -> Decoder' c i e f a
withCursor' =
  Decoder'

-- |
-- Execute a given 'DecoderResultT'.
--
-- If you're building your own decoder structure, this function will take care
-- of the 'CursorHistory'' and error handling (via 'ExceptT').
--
runDecoderResultT
  :: Monad f
  => DecodeResultT i DecodeError f a
  -> f (Either (DecodeError, CursorHistory' i) a)
runDecoderResultT =
  fmap (\(e, hist) -> first (,hist) e)
  . flip runStateT (CursorHistory' mempty)
  . runExceptT
  . runDecodeResult

-- |
-- Record a move on the zipper and the index of the position where the move
-- occured.
--
recordZipperMove :: MonadState (CursorHistory' i) m => ZipperMove -> i -> m ()
recordZipperMove dir i = L._Wrapped %= (`L.snoc` (dir, i))

-- |
-- Attempt a 'Decoder' action that might fail and return a 'Maybe' value
-- instead.
--
try :: MonadError e m => m a -> m (Maybe a)
try d = catchError (pure <$> d) (const (pure Nothing))

-- |
-- Build the basis for a 'Decoder' based on a 'Prism''.
--
prismDOrFail'
  :: ( AsDecodeError e
     , MonadError e f
     )
  => e
  -> L.Prism' a b
  -> Decoder' c i e f a
  -> c
  -> DecodeResultT i e f b
prismDOrFail' e p d c =
  runDecoder' (L.preview p <$> d) c <!?> e

-- | Try to decode a 'Text' value from some 'Json' or value.
text' :: AsJType a ws a => a -> Maybe Text
text' = L.preview (_JStr . _1 . L.re _JString)

-- | Try to decode a 'String' value from some 'Json' or value.
string' :: AsJType a ws a => a -> Maybe String
string' = L.preview (_JStr . _1 . _Wrapped . L.to (V.toList . V.map (_JChar L.#)))

-- | Decoder for a 'Char' value that cannot contain values in the range U+D800
-- to U+DFFF. This decoder will fail if the 'Char' is outside of this range.
boundedChar' :: AsJType a ws a => a -> Maybe Char
boundedChar' = L.preview (_JStr . _1 . _Wrapped . L._head) >=> jCharToUtf8Char

-- | Decoder for a Haskell 'Char' value whose values represent Unicode
-- (or equivalently ISO/IEC 10646) characters
unboundedChar' :: AsJType a ws a => a -> Maybe Char
unboundedChar' = L.preview (_JStr . _1 . _Wrapped . L._head . L.re _JChar)

-- | Try to decode a 'Scientific' value from some 'Json' or value.
scientific' :: AsJType a ws a => a -> Maybe Scientific
scientific' = L.preview (_JNum . _1) >=> jNumberToScientific

-- | Try to decode a bounded 'Integral n => n' value from some 'Json' value.
integral' :: (Bounded i , Integral i , AsJType a ws a) => a -> Maybe i
integral' = scientific' >=> Sci.toBoundedInteger

-- | Try to decode an 'Int' from some 'Json' value
int' :: AsJType a ws a => a -> Maybe Int
int' = integral'

-- | Try to decode a 'Bool' from some 'Json' value
bool' :: AsJType a ws a => a -> Maybe Bool
bool' = L.preview (_JBool . _1)

-- | Try to decode a 'null' value from some 'Json' value
null' :: AsJType a ws a => a -> Maybe ()
null' a = L.preview _JNull a $> ()

-- | Combined with another decoder function 'f', try to decode a list of 'a' values.
--
-- @
-- array' int' :: Json -> [Int]
-- @
--
array' :: AsJType a ws a => (a -> Maybe b) -> a -> [b]
array' f a = Wither.mapMaybe f (a L.^.. _JArr . _1 . L.folded)

-- |
-- Try to decode a JSON Object into it's representative list of
-- tuples '(key, value)'. The JSON RFC does not specify that an object must
-- contain unique keys. We do not enforce unique keys during the decoding
-- process and leave it to the user to decide if, and how, they would like to
-- handle this situation.
--
objTuples'
  :: ( Applicative f
     , AsJType a ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f [(k, b)]
objTuples' kF vF a =
  traverse g (a L.^.. _JObj . _1 . _Wrapped . L.to toList . L.folded)
  where
    g ja = liftA2 (,)
      (ja L.^. jsonAssocKey . L.to kF)
      (ja L.^. jsonAssocVal . L.to vF)

-- | Generalised moving decoder function.
--
-- Starting from the given cursor position, try to move in the direction
-- specified by the given cursor function. Attempting to decode each item at each
-- position using the given 'Decoder', until the movement is unsuccessful.
--
-- The following could be used to leverage the 'Snoc' instance of '[]' to build '[Int]'.
--
-- @
-- intList :: Monad f => JCurs -> DecodeResult f [Int]
-- intList = directedConsumption' snoc moveRight1 int
-- @
--
foldCursor'
  :: Monad f
  => b
  -> (b -> a -> b)
  -> (c -> DecodeResultT i e f c)
  -> Decoder' c i e f a
  -> c
  -> DecodeResultT i e f b
foldCursor' empty scons mvCurs elemD =
  go empty
  where
    go acc cur = do
      acc' <- scons acc <$> runDecoder' elemD cur

      try (mvCurs cur) >>= maybe
        (pure acc')
        (go acc')

-- |
-- Provide a generalised and low level way of turning a JSON object into a
-- 'Map', without enforcing a choice of how we select keys.
--
mapKeepingF
  :: ( Ord k
     , Applicative f
     , AsJType a ws a
     )
  => (t -> Maybe v -> Maybe v)
  -> (JString -> f k)
  -> (a -> f t)
  -> a
  -> f (Map k v)
mapKeepingF f kF vF a =
  foldr (\(k,v) -> Map.alter (f v) k) Map.empty <$> objTuples' kF vF a

-- |
-- Turn a JSON object into a 'Map' by keeping the *first* occurence of any
-- duplicate keys that are encountered.
--
mapKeepingFirst
  :: ( Ord k
     , Applicative f
     , AsJType a ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f (Map k b)
mapKeepingFirst =
  mapKeepingF (\v -> (<|> Just v))

-- |
-- Turn a JSON object into a 'Map' by keeping the *last* occurence of any
-- duplicate keys that are encountered.
--
mapKeepingLast
  :: ( Ord k
     , Applicative f
     , AsJType a ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f (Map k b)
mapKeepingLast =
  mapKeepingF (\v -> (Just v <|>))
