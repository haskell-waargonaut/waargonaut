{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
module Waargonaut.Decode.DecodeResult
  ( CursorHistory' (..)
  , DecodeResultT (..)

  , Decoder' (..)
  , withCursor'
  --
  , DecodeError (..)
  , Mv (..)

  , runDecoderResultT
  , try

  -- * General Decoder Functions
  , null'
  , int'
  , text'
  , string'
  , boolean'
  , array'
  , integral'
  , scientific'
  , tuples'
  , mapKeepingF
  , mapKeepingFirst
  , mapKeepingLast

  ) where

import           GHC.Word                  (Word64)
import           Numeric.Natural           (Natural)

import           Control.Applicative       (liftA2, (<|>))
import           Control.Lens              (Rewrapped, Wrapped (..), _1,
                                            _Wrapped)
import qualified Control.Lens              as L

import           Control.Monad             ((>=>))
import           Control.Monad.Except      (ExceptT (..), MonadError (..),
                                            runExceptT)
import           Control.Monad.State       (MonadState (..), StateT (..))

import           Data.Bifunctor            (first)
import           Data.Functor              (Functor, ($>))
import           Data.Sequence             (Seq)

import           Data.Text                 (Text)

import           Data.Map                  (Map)
import qualified Data.Map                  as Map

import qualified Data.Vector as V

import qualified Data.Witherable           as Wither

import           Data.Scientific           (Scientific)
import qualified Data.Scientific           as Sci

import           Waargonaut.Types          (AsJTypes (..), JNumber, JString,
                                            jNumberToScientific, jsonAssocKey,
                                            jsonAssocVal, _JChar, _JStringText)
import           Waargonaut.Types.CommaSep (toList)

data DecodeError
  = ConversionFailure Text
  | KeyDecodeFailed Text
  | KeyNotFound Text
  | FailedToMove Mv
  | NumberOutOfBounds JNumber
  | InputOutOfBounds Word64
  | ParseFailed Text
  deriving (Show, Eq)

data Mv
  = U
  | D
  | DAt Text
  | L Natural
  | R Natural
  deriving (Show, Eq)

newtype CursorHistory' i = CursorHistory' (Seq (Mv, i))
  deriving (Eq, Show)

instance CursorHistory' i ~ t => Rewrapped (CursorHistory' i) t

instance Wrapped (CursorHistory' i) where
  type Unwrapped (CursorHistory' i) = Seq (Mv, i)
  _Wrapped' = L.iso (\(CursorHistory' x) -> x) CursorHistory'
  {-# INLINE _Wrapped' #-}

newtype DecodeResultT i f e a = DecodeResultT
  { runDecodeResult :: ExceptT e (StateT (CursorHistory' i) f) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (CursorHistory' i)
           , MonadError e
           )

newtype Decoder' c i f e a = Decoder'
  { runDecoder' :: c -> DecodeResultT i f e a
  }

withCursor'
  :: Monad f
  => (c -> DecodeResultT i f e a)
  -> Decoder' c i f e a
withCursor' =
  Decoder'

runDecoderResultT
  :: ( Num i
     , Monad f
     )
  => DecodeResultT i f DecodeError a
  -> f (Either (DecodeError, CursorHistory' i) a)
runDecoderResultT =
  fmap (\(e, hist) -> first (,hist) e)
  . flip runStateT (CursorHistory' mempty)
  . runExceptT
  . runDecodeResult

try :: MonadError e m => m a -> m (Maybe a)
try d = catchError (pure <$> d) (const (pure Nothing))

text' :: AsJTypes a digit ws a => a -> Maybe Text
text' = L.preview (_JStr . _1 . _JStringText)

string' :: AsJTypes a digit ws a => a -> Maybe String
string' a = a L.^? _JStr . _1 . _Wrapped . L.to (V.toList . V.map (_JChar L.#))

scientific' :: AsJTypes a digit ws a => a -> Maybe Scientific
scientific' = L.preview (_JNum . _1) >=> jNumberToScientific

integral' :: (Bounded i , Integral i , AsJTypes a digit ws a) => a -> Maybe i
integral' = scientific' >=> Sci.toBoundedInteger

int' :: AsJTypes a digit ws a => a -> Maybe Int
int' = integral'

boolean' :: AsJTypes a digit ws a => a -> Maybe Bool
boolean' = L.preview (_JBool . _1)

null' :: AsJTypes a digit ws a => a -> Maybe ()
null' a = L.preview _JNull a $> ()

array' :: AsJTypes a digit ws a => (a -> Maybe b) -> a -> [b]
array' f a = Wither.mapMaybe f (a L.^.. _JArr . _1 . L.folded)

tuples'
  :: ( Applicative f
     , AsJTypes a digit ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f [(k, b)]
tuples' kF vF a =
  traverse g (a L.^.. _JObj . _1 . _Wrapped . L.to toList . L.folded)
  where
    g ja = liftA2 (,)
      (ja L.^. jsonAssocKey . L.to kF)
      (ja L.^. jsonAssocVal . L.to vF)

mapKeepingF
  :: ( Ord k
     , Applicative f
     , AsJTypes a digit ws a
     )
  => (t -> Maybe a1 -> Maybe a1)
  -> (JString -> f k)
  -> (a -> f t)
  -> a
  -> f (Map k a1)
mapKeepingF f kF vF a =
  foldr (\(k,v) -> Map.alter (f v) k) Map.empty <$> tuples' kF vF a

mapKeepingFirst
  :: ( Ord k
     , Applicative f
     , AsJTypes a digit ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f (Map k b)
mapKeepingFirst =
  mapKeepingF (\v -> (<|> Just v))

mapKeepingLast
  :: ( Ord k
     , Applicative f
     , AsJTypes a digit ws a
     )
  => (JString -> f k)
  -> (a -> f b)
  -> a
  -> f (Map k b)
mapKeepingLast =
  mapKeepingF (\v -> (Just v <|>))
