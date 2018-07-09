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
  ) where

import           GHC.Word             (Word64)
import           Numeric.Natural      (Natural)

import           Control.Lens         (Rewrapped, Wrapped (..))
import qualified Control.Lens         as L

import           Control.Monad.Except (ExceptT (..), MonadError (..),
                                       runExceptT)
import           Control.Monad.State  (MonadState (..), StateT (..))

import           Data.Bifunctor       (first)
import           Data.Functor         (Functor)
import           Data.Sequence        (Seq)

import           Data.Text            (Text)

import           Waargonaut.Types     (JNumber)

data DecodeError
  = ConversionFailure Text
  | KeyDecodeFailed Text
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

try
  :: Monad f
  => DecodeResultT i f e a
  -> DecodeResultT i f e (Maybe a)
try d =
  catchError (pure <$> d) (const (pure Nothing))
