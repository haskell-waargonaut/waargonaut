{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
--
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Waargonaut.Zipper where

import           Control.Lens                  (Bazaar', Indexed, LensLike',
                                                makeWrapped, modifying, over,
                                                snoc, view, _Left, _Wrapped)
import           Control.Lens.Internal.Indexed (Indexing)

import Control.Monad ((<=<))

import           Control.Monad.Except          (ExceptT, MonadError, runExceptT,
                                                throwError)
import           Control.Monad.Reader          (MonadReader, ReaderT,
                                                runReaderT,ask)
import           Control.Monad.State           (MonadState, State, runState)

import           Control.Error.Util

import           Data.Sequence                 (Seq)

import           Data.Functor.Identity         (Identity (..))

import           Data.Text                     (Text)
import Data.Scientific (toBoundedInteger)

import           Waargonaut.Types
import           Waargonaut.Types.CommaSep
import           Waargonaut.Types.JNumber
import           Waargonaut.Types.JObject

import           Control.Zipper


-- false
jboolFalse :: Json
jboolFalse = Json (JBool False emptyWS)

jboolTrue :: Json
jboolTrue = Json (JBool True emptyWS)

-- {"abc":false}
obj :: Json
obj = Json (JObj (JObject cs) emptyWS)
  where
    js = JAssocKey $ JString
      [ UnescapedJChar (JCharUnescaped 'a')
      , UnescapedJChar (JCharUnescaped 'b')
      , UnescapedJChar (JCharUnescaped 'c')
      ]

    cs = CommaSeparated emptyWS (
      Just (
          Elems [Elem (JAssoc js emptyWS emptyWS jboolFalse) (Identity (Comma, WS []))] (
              Elem (JAssoc js emptyWS emptyWS jboolTrue) Nothing
              )
          )
      )


data Dir
  = Lft
  | Rgt
  | Up
  | Into Text
  | IntoIdx Int
  deriving (Show, Eq)

data DecodeError
  = ConversionFailure Text
  | MissingElement Dir
  | FailedToMove Dir
  | IntOutOfBounds
  deriving (Show, Eq)

newtype CursorHistory =
  CursorHistory (Seq Dir)
  deriving Show
makeWrapped ''CursorHistory


type JCursor h a =
  h :>> a

type JCursorMove s a =
  LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a

newtype DecodeResult a = DecodeResult
  { unDecodeResult :: ReaderT Json (ExceptT DecodeError (State CursorHistory)) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Json
           , MonadError DecodeError
           , MonadState CursorHistory
           )

moveAndKeepHistory
  :: Dir
  -> Maybe (JCursor h s)
  -> DecodeResult (JCursor h s)
moveAndKeepHistory dir mCurs = do
  a <- either throwError pure . note (FailedToMove dir) $ mCurs
  modifying _Wrapped (`snoc` dir)
  pure a

newCursor
  :: DecodeResult (JCursor Top Json)
newCursor =
  zipper <$> ask

into
  :: Text
  -> JCursorMove s a
  -> JCursor h s
  -> DecodeResult (JCursor h s :>> a)
into tgt l =
  moveAndKeepHistory (Into tgt) . withins l

up
  :: JCursor (JCursor h s) a
  -> DecodeResult (JCursor h s)
up =
  moveAndKeepHistory Up . pure . upward

stepLeft
  :: JCursor h a
  -> DecodeResult (JCursor h a)
stepLeft =
  moveAndKeepHistory Lft . leftward

stepRight
  :: JCursor h a
  -> DecodeResult (JCursor h a)
stepRight =
  moveAndKeepHistory Rgt . rightward

withCursor
  :: (a -> Either Text b)
  -> JCursor h a
  -> DecodeResult b
withCursor f =
  either (throwError . ConversionFailure) pure . f . view focus

integral
  :: ( Bounded i
     , Integral i
     )
  => JCursor h JNumber
  -> DecodeResult i
integral = either throwError pure
  . note IntOutOfBounds
  . (toBoundedInteger <=< jNumberToScientific)
  . view focus

runDecode
  :: Json
  -> DecodeResult a
  -> Either (CursorHistory, DecodeError) a
runDecode j =
  let
    f = flip runState (CursorHistory mempty)
      . runExceptT
      . flip runReaderT j
      . unDecodeResult

    g (r, z) =
      over _Left (z,) r
  in
    g . f
