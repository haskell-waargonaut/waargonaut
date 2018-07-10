{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
--
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Waargonaut.Decode where

import           Numeric.Natural                (Natural)

import           Control.Monad.Except           (MonadError)
import           Control.Monad.State            (MonadState)

import           Control.Lens                   (Bazaar', LensLike', (%=), (^.),
                                                 (^?))
import qualified Control.Lens                   as L
import           Control.Lens.Internal.Indexed  (Indexed, Indexing)

import           Data.Text                      (Text)

import           Control.Error.Util             (note)
import           Control.Monad.Error.Hoist      ((<%?>), (<?>))

import           Control.Zipper                 ((:>>))
import qualified Control.Zipper                 as Z

import           Text.Parser.Char               (CharParsing)
import           Text.Parser.Combinators        (Parsing)

import           Waargonaut.Types               (AsJTypes, JAssoc, Json,
                                                 MapLikeObj)

import qualified Waargonaut.Types               as WT
import           Waargonaut.Types.CommaSep      (Elems)
import qualified Waargonaut.Types.CommaSep      as WT

import           Waargonaut.Decode.DecodeResult (CursorHistory' (..),
                                                 DecodeError (..),
                                                 DecodeResultT, Mv (..),
                                                 try,
                                                 runDecoderResultT)

import qualified Waargonaut.Decode.DecodeResult as DR

newtype CursorHistory = CursorHist
  { unCursorHist :: CursorHistory' Int
  }
  deriving (Show)

newtype DecodeResult f a = DecodeResult
  { unDecodeResult :: DecodeResultT Int f DecodeError a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (CursorHistory' Int)
           , MonadError DecodeError
           )

type JCursorMove s a =
  LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a

type JCursor h a =
  h :>> a

type Decoder f a =
  forall h. JCursor h Json -> DecodeResult f a

decodeToJson :: (Monad m, CharParsing m, Parsing m) => (m Json -> m Json) -> m Json
decodeToJson f = f WT.parseWaargonaut

runDecoderResult
  :: Monad f
  => DecodeResult f a
  -> f (Either (DecodeError, CursorHistory) a)
runDecoderResult =
  L.over (L.mapped . L._Left . L._2) CursorHist
  . runDecoderResultT
  . unDecodeResult

moveAndKeepHistory
  :: Monad f
  => Mv
  -> Maybe (JCursor h s)
  -> DecodeResult f (JCursor h s)
moveAndKeepHistory dir mCurs = do
  a <- mCurs <?> FailedToMove dir
  a <$ (L._Wrapped %= (`L.snoc` (dir, Z.tooth a)))

into
  :: Monad f
  => Text
  -> JCursorMove s a
  -> JCursor h s
  -> DecodeResult f (JCursor (JCursor h s) a)
into tgt l =
  moveAndKeepHistory (DAt tgt) . Z.withins l

up
  :: Monad f
  => JCursor (JCursor h s) a
  -> DecodeResult f (JCursor h s)
up =
  moveAndKeepHistory U . pure . Z.upward

moveLeftN
  :: Monad f
  => Natural
  -> JCursor h a
  -> DecodeResult f (JCursor h a)
moveLeftN n cur =
  moveAndKeepHistory (L n) (Z.jerks Z.leftward (fromIntegral n) cur)

moveRightN
  :: Monad f
  => Natural
  -> JCursor h a
  -> DecodeResult f (JCursor h a)
moveRightN n cur =
  moveAndKeepHistory (R n) (Z.jerks Z.rightward (fromIntegral n) cur)

moveLeft1
  :: Monad f
  => JCursor h a
  -> DecodeResult f (JCursor h a)
moveLeft1 =
  moveLeftN 1

moveRight1
  :: Monad f
  => JCursor h a
  -> DecodeResult f (JCursor h a)
moveRight1 =
  moveRightN 1

atCursor
  :: Monad f
  => (a -> Either Text b)
  -> JCursor h a
  -> DecodeResult f b
atCursor f c =
  c ^. Z.focus . L.to f <%?> ConversionFailure

valAt
  :: ( Monoid ws
     , Monad f
     )
  => Text
  -> (JCursor (JCursor h (MapLikeObj ws a)) a -> DecodeResult f b)
  -> JCursor h (MapLikeObj ws a)
  -> DecodeResult f b
valAt k f c =
  into k (L.at k . L._Just) c >>= f

toKey
  :: ( AsJTypes s digit ws s
     , Monad f
     )
  => Text
  -> JCursor h s
  -> DecodeResult f (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s)
toKey k c =
  let
    c' = Z.within intoElems c >>= Z.within traverse >>= shuffleToKey
  in
    moveAndKeepHistory (DAt k) (c' >>= Z.within WT.jsonAssocVal)
  where
    shuffleToKey cu = Z.within WT.jsonAssocKey cu ^? L._Just . Z.focus . WT._JStringText >>= \k' ->
      if k' /= k then Z.rightward cu >>= shuffleToKey else Just cu

    intoElems = WT._JObj . L._1 . L._Wrapped . WT._CommaSeparated . L._2 . L._Just

fromKey
  :: ( Monad f
     , AsJTypes s digit ws s
     )
  => Text
  -> (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s -> DecodeResult f b)
  -> JCursor h s
  -> DecodeResult f b
fromKey k d c =
  toKey k c >>= d

integral
  :: ( Bounded i
     , Integral i
     , Monad f
     , AsJTypes a digit ws a
     )
  => JCursor h a
  -> DecodeResult f i
integral =
  atCursor (note "Integral" . DR.integral')

int
  :: ( AsJTypes a digit ws a
     , Monad f
     )
  => JCursor h a
  -> DecodeResult f Int
int =
  atCursor (note "Int" . DR.int')

boolean
  :: ( AsJTypes a digit ws a
     , Monad f
     )
  => JCursor h a
  -> DecodeResult f Bool
boolean =
  atCursor (note "Bool" . DR.boolean')

text
  :: ( AsJTypes a digit ws a
     , Monad f
     )
  => JCursor h a
  -> DecodeResult f Text
text =
  atCursor (note "Text" . DR.text')

arrayOf
  :: Monad f
  => Decoder f b
  -> h :>> Json
  -> DecodeResult f [b]
arrayOf elemD c =
  moveAndKeepHistory D (Z.within WT.json c) >>= go mempty
  where
    go acc cur = do
      r <- (:acc) <$> elemD cur
      try (moveAndKeepHistory (R 1) (Z.rightward cur))
        >>= maybe (pure r) (go r)
