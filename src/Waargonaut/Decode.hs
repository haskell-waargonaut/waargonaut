{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Waargonaut.Decode where

import           Numeric.Natural                (Natural)

import           Control.Lens                   (Bazaar', LensLike', (%=), (^.))
import qualified Control.Lens                   as L
import           Control.Lens.Internal.Indexed  (Indexed, Indexing)

import           Data.Scientific                (toBoundedInteger)
import           Data.Text                      (Text)

import           Control.Monad.Error.Hoist      ((<%?>), (<?>))

import           Control.Zipper                 ((:>>))
import qualified Control.Zipper                 as Z

import           Waargonaut.Types               (JNumber, jNumberToScientific)

import           Waargonaut.Decode.DecodeResult (CursorHistory' (..),
                                                 DecodeError (..),
                                                 DecodeResultT, Mv (..),
                                                 runDecoderResultT)

newtype CursorHistory = CursorHist 
  { unCursorHist :: CursorHistory' Int
  }

newtype DecodeResult f a = DecodeResult
  { unDecodeResult :: DecodeResultT Int f DecodeError a
  }

type JCursorMove s a =
  LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a

type JCursor h a =
  h :>> a

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
moveAndKeepHistory dir mCurs = DecodeResult $ do
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

withCursor
  :: Monad f
  => (a -> Either Text b)
  -> JCursor h a
  -> DecodeResult f b
withCursor f c = DecodeResult $
  c ^. Z.focus . L.to f <%?> ConversionFailure

integral
  :: ( Bounded i
     , Integral i
     , Monad f
     )
  => JCursor h JNumber
  -> DecodeResult f i
integral c =
  let
    v = c ^. Z.focus
  in
    DecodeResult $ (toBoundedInteger =<< jNumberToScientific v) <?> NumberOutOfBounds v
