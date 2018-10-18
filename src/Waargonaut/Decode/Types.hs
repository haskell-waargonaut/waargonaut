{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Waargonaut.Decode.Types
  ( ParseFn
  , SuccinctCursor
  , CursorHistory
  , Decoder (..)
  , DecodeResult (..)
  , JCurs (..)
  ) where

import           Control.Lens                          (Rewrapped, Wrapped (..),
                                                        iso)
import           Control.Monad.Except                  (MonadError)
import           Control.Monad.Morph                   (MFunctor (..),
                                                        MMonad (..))
import           Control.Monad.Reader                  (MonadReader,
                                                        ReaderT (..))
import           Control.Monad.State                   (MonadState)
import           Control.Monad.Trans.Class             (MonadTrans (lift))

import           GHC.Word                              (Word64)

import           Data.Functor                          (Functor)

import           Data.ByteString                       (ByteString)
import           Data.Vector.Storable                  (Vector)

import           HaskellWorks.Data.BalancedParens      (SimpleBalancedParens)
import           HaskellWorks.Data.Json.Cursor         (JsonCursor (..))
import           HaskellWorks.Data.Positioning         (Count)
import           HaskellWorks.Data.RankSelect.Poppy512 (Poppy512)

import           Waargonaut.Decode.Internal            (CursorHistory',
                                                        DecodeError (..),
                                                        DecodeResultT (..))

import           Waargonaut.Types                      (Json)

type CursorHistory =
  CursorHistory' Count

type SuccinctCursor =
  JsonCursor ByteString Poppy512 (SimpleBalancedParens (Vector Word64))

type ParseFn =
  ByteString -> Either DecodeError Json

newtype Decoder f a = Decoder
  { runDecoder :: ParseFn -> JCurs -> DecodeResultT Count DecodeError f a
  }
  deriving Functor

instance Monad f => Applicative (Decoder f) where
  pure       = pure
  aToB <*> a = Decoder $ \p c ->
    runDecoder aToB p c <*> runDecoder a p c

instance Monad f => Monad (Decoder f) where
  return      = pure
  a >>= aToFb = Decoder $ \p c -> do
    r <- runDecoder a p c
    runDecoder (aToFb r) p c

instance MFunctor Decoder where
  hoist nat (Decoder pjdr) = Decoder (\p -> hoist nat . pjdr p)

newtype JCurs = JCurs
  { unJCurs :: SuccinctCursor
  }

instance JCurs ~ t => Rewrapped JCurs t

instance Wrapped JCurs where
  type Unwrapped JCurs = SuccinctCursor
  _Wrapped' = iso unJCurs JCurs

newtype DecodeResult f a = DecodeResult
  { unDecodeResult :: ReaderT ParseFn (DecodeResultT Count DecodeError f) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ParseFn
           , MonadError DecodeError
           , MonadState CursorHistory
           )

instance MonadTrans DecodeResult where
  lift = DecodeResult . lift . lift

instance MFunctor DecodeResult where
  hoist nat (DecodeResult dr) = DecodeResult (hoist (hoist nat) dr)

instance MMonad DecodeResult where
  embed f (DecodeResult dr) = DecodeResult . ReaderT $ \p ->
    embed (flip runReaderT p . unDecodeResult . f) $ runReaderT dr p
