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

import           Numeric.Natural               (Natural)

import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (MonadError)
import           Control.Monad.State           (MonadState)

import           Control.Lens                  (AsEmpty (..), Bazaar', Cons,
                                                LensLike', (^.), (^?))
import qualified Control.Lens                  as L
import           Control.Lens.Internal.Indexed (Indexed, Indexing)

import           Control.Error.Util            (note)
import           Control.Monad.Error.Hoist     ((<%?>), (<?>))

import           Control.Zipper                ((:>>))
import qualified Control.Zipper                as Z


import           Data.Text                     (Text)

import           Data.Scientific               (Scientific)

import           Text.Parser.Char              (CharParsing)
import           Text.Parser.Combinators       (Parsing)

import           Waargonaut.Types              (AsJTypes, JAssoc, Json)

import qualified Waargonaut.Types              as WT
import           Waargonaut.Types.CommaSep     (Elems)
import qualified Waargonaut.Types.CommaSep     as WT

import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeError (..), DecodeResultT,
                                                Decoder' (..), Mv (..),
                                                runDecoderResultT, try)

import qualified Waargonaut.Decode.Internal    as DR

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
  forall h. Decoder' (JCursor h Json) Int f DecodeError a

withCursor
  :: Monad f
  => (forall h. JCursor h Json -> DecodeResult f a)
  -> Decoder f a
withCursor f =
  Decoder' (unDecodeResult . f)

runDecoder
  :: Decoder f a
  -> JCursor h Json
  -> DecodeResult f a
runDecoder f =
  DecodeResult . DR.runDecoder' f

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
  a <$ DR.recordMv dir (Z.tooth a)

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
  => Text
  -> (Json -> Maybe b)
  -> Decoder f b
atCursor t f = withCursor $ \c -> do
  b <- c ^. Z.focus . L.to (note t . f) <%?> ConversionFailure
  b <$ DR.recordMv (Item t) (Z.tooth c)

toKey
  :: ( AsJTypes s digit ws s
     , Monad f
     )
  => Text
  -> JCursor h s
  -> DecodeResult f (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s)
toKey k =
  moveAndKeepHistory (DAt k) . moveToVal
  where
    moveToVal = Z.within intoElems
      >=> Z.within traverse
      >=> shuffleToKey
      >=> Z.within WT.jsonAssocVal

    shuffleToKey cu = Z.within WT.jsonAssocKey cu ^? L._Just . Z.focus . WT._JStringText >>= \k' ->
      if k' /= k then Z.rightward cu >>= shuffleToKey else Just cu

    intoElems = WT._JObj . L._1 . L._Wrapped . WT._CommaSeparated . L._2 . L._Just

-- (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s -> DecodeResult f b)
fromKey
  :: ( Monad f
     )
  => Text
  -> Decoder f b
  -> JCursor h Json
  -> DecodeResult f b
fromKey k d =
  toKey k >=> runDecoder d

scientific :: Monad f => Decoder f Scientific
scientific = atCursor "Scientific" DR.scientific'

integral :: (Bounded i, Integral i, Monad f) => Decoder f i
integral = atCursor "Integral" DR.integral'

int :: Monad f => Decoder f Int
int = atCursor "Int" DR.int'

boolean :: Monad f => Decoder f Bool
boolean = atCursor "Bool" DR.boolean'

text :: Monad f => Decoder f Text
text = atCursor "Text" DR.text'

string :: Monad f => Decoder f String
string = atCursor "String" DR.string'

arrayOfCons
  :: ( Monad f
     , AsEmpty s
     , Cons s s a a
     )
  => Decoder f a
  -> JCursor h Json
  -> DecodeResult f s
arrayOfCons elemD c =
  moveAndKeepHistory D (Z.within WT.json c) >>= go (_Empty L.# ())
  where
    go acc cur = do
      r <- (`L.cons` acc) <$> runDecoder elemD cur
      try (moveAndKeepHistory (R 1) (Z.rightward cur))
        >>= maybe (pure r) (go r)

arrayOf :: Monad f => Decoder f b -> Decoder f [b]
arrayOf d = withCursor (arrayOfCons d)
