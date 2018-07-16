{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- | Types and functions to convert Json values into your data types.
module Waargonaut.Decode
  (
    CursorHistory (..)
  , DecodeResult (..)

    -- * Type aliases
  , JCursorMove
  , JCursor
  , Decoder

  , decodeToJson

    -- * Decoder creation
  , withCursor

    -- * Decoder execution
  , runDecoder
  , runDecoderResult

    -- * Cursor movement
  , into
  , up
  , moveLeftN
  , moveLeft1
  , moveRightN
  , moveRight1
  , moveToKey

    -- * Decode at Cursor
  , fromKey
  , atCursor

    -- * Provided decoders
  , scientific
  , integral
  , int
  , boolean
  , text
  , string
  , arrayOfCons
  , arrayOf

  ) where


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

import           Waargonaut.Types              (AsJType, Elems, JAssoc, Json)

import qualified Waargonaut.Types              as WT

import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeError (..), DecodeResultT,
                                                Decoder' (..), Mv (..),
                                                runDecoderResultT, try)

import qualified Waargonaut.Decode.Internal    as DR

-- | Wrapper for our 'CursorHistory'' to define our index as being an 'Int'.
--
newtype CursorHistory = CursorHist
  { unCursorHist :: CursorHistory' Int
  }
  deriving (Show, Eq)

-- | Provide some of the type parameters that the underlying 'DecodeResultT'
-- requires. This contains the state and error management as we walk around our
-- zipper and decode our JSON input.
--
newtype DecodeResult f a = DecodeResult
  { unDecodeResult :: DecodeResultT Int f DecodeError a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (CursorHistory' Int)
           , MonadError DecodeError
           )

-- | Type alias to describe the lens that may be given to a zipper movement
-- function to more directly target something within the 'Json' data structure.
--
type JCursorMove s a =
  LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a

-- | This is an alias to help explain a type from the zipper that is used to move
-- around the 'Json' data structure. 'JCursor h a' represents a "cursor" that is
-- currently located on a thing of type 'a', having previously been on a thing
-- of type 'h'.
--
-- This type will grow as a form of "breadcrumb" trail as the cursor moves
-- through the data structure. It may be used interchangably with 'h :>> a' from
-- the 'Control.Zipper' module.
--
type JCursor h a =
  h :>> a

-- | A shorthand description of our 'Decoder' type that is used directly to
-- convert 'Json' structures to other data types.
--
type Decoder f a =
  forall h. Decoder' (JCursor h Json) Int f DecodeError a

-- | Function to define a 'Decoder' for a specific data type.
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
-- We can use 'withCursor' to write a decoder that will be given a cursor that
-- we can use to build the data types that we need.
--
-- @
-- imageDecoder :: Monad f => Decoder f Image
-- imageDecoder = withCursor $ \\curs -> Image
--   \<$> D.fromKey \"Width\" D.int curs
--   \<*> D.fromKey \"Height\" D.int curs
--   \<*> D.fromKey \"Title\" D.text curs
--   \<*> D.fromKey \"Animated\" D.boolean curs
--   \<*> D.fromKey \"IDs\" intArray curs
-- @
--
-- It's up to you to provide a cursor that is at the correct position for a
-- 'Decoder' to operate, but building decoders in this way simplifies creating
-- decoders for larger structures, as the smaller pieces contain fewer
-- assumptions. This encourages greater reuse of decoders and simplifies the
-- debugging process.
--
withCursor
  :: Monad f
  => (forall h. JCursor h Json -> DecodeResult f a)
  -> Decoder f a
withCursor f =
  Decoder' (unDecodeResult . f)

-- | Run a 'Decoder f a' using a 'JCursor' to try to convert it into the data
-- type described by the 'Decoder'.
--
runDecoder
  :: Decoder f a
  -> JCursor h Json
  -> DecodeResult f a
runDecoder f =
  DecodeResult . DR.runDecoder' f

-- | Execute a 'DecodeResult' to determine if the process has been successful,
-- providing a descriptive error and the path history of the cursor movements to
-- assist in debugging any failures.
--
runDecoderResult
  :: Monad f
  => DecodeResult f a
  -> f (Either (DecodeError, CursorHistory) a)
runDecoderResult =
  L.over (L.mapped . L._Left . L._2) CursorHist
  . runDecoderResultT
  . unDecodeResult

-- | Something to plug your chosen parser into. This function/design is likely to change.
decodeToJson :: (Monad m, CharParsing m, Parsing m) => (m Json -> m Json) -> m Json
decodeToJson f = f WT.parseWaargonaut

-- Helper function that takes a given attempt to move the cursor to a new
-- location, throwing the 'FailedToMove' error if it fails, or recording the new
-- position and returning the new position.
moveAndKeepHistory
  :: Monad f
  => Mv
  -> Maybe (JCursor h s)
  -> DecodeResult f (JCursor h s)
moveAndKeepHistory dir mCurs = do
  a <- mCurs <?> FailedToMove dir
  a <$ DR.recordMv dir (Z.tooth a)

-- | Using a given 'LensLike', try to step down into the 'Json' data structure
-- to the location targeted by the lens.
--
-- This can be used to move large steps over the data structure, or more
-- precisely to specific keys at deeper levels. On a successful step, the
-- history will be recorded as a single step into the thing described by the
-- 'Text' input.
--
into
  :: Monad f
  => Text
  -> JCursorMove s a
  -> JCursor h s
  -> DecodeResult f (JCursor (JCursor h s) a)
into tgt l =
  moveAndKeepHistory (DAt tgt) . Z.within l

-- | Attempt to step one level "up" from the current cursor location.
up
  :: Monad f
  => JCursor (JCursor h s) a
  -> DecodeResult f (JCursor h s)
up =
  moveAndKeepHistory U . pure . Z.upward

-- | From the current cursor location, try to move 'n' steps to the left.
moveLeftN
  :: Monad f
  => Natural
  -> JCursor h a
  -> DecodeResult f (JCursor h a)
moveLeftN n cur =
  moveAndKeepHistory (L n) (Z.jerks Z.leftward (fromIntegral n) cur)

-- | From the current cursor location, try to move 'n' steps to the right.
moveRightN
  :: Monad f
  => Natural
  -> JCursor h a
  -> DecodeResult f (JCursor h a)
moveRightN n cur =
  moveAndKeepHistory (R n) (Z.jerks Z.rightward (fromIntegral n) cur)

-- | From the current cursor location, try to move 1 step to the left.
moveLeft1
  :: Monad f
  => JCursor h a
  -> DecodeResult f (JCursor h a)
moveLeft1 =
  moveLeftN 1

-- | From the current cursor location, try to move 1 step to the right.
moveRight1
  :: Monad f
  => JCursor h a
  -> DecodeResult f (JCursor h a)
moveRight1 =
  moveRightN 1

-- | Provide a 'conversion' function and create a 'Decoder' that uses the
-- current cursor and runs the given function. Fails with 'ConversionFailure' and
-- the given 'Text' description.
atCursor
  :: Monad f
  => Text
  -> (Json -> Maybe b)
  -> Decoder f b
atCursor t f = withCursor $ \c -> do
  b <- c ^. Z.focus . L.to (note t . f) <%?> ConversionFailure
  b <$ DR.recordMv (Item t) (Z.tooth c)

-- | From the current cursor position, try to move to the value for the first
-- occurence of that key. This move expects that you've positioned the cursor on an
-- object.
moveToKey
  :: ( AsJType s digit ws s
     , Monad f
     )
  => Text
  -> JCursor h s
  -> DecodeResult f (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s)
moveToKey k =
  moveAndKeepHistory (DAt k) . ( Z.within intoElems
                                 >=> Z.within traverse
                                 >=> shuffleToKey
                                 >=> Z.within WT.jsonAssocVal
                               )
  where
    shuffleToKey cu = Z.within WT.jsonAssocKey cu ^? L._Just . Z.focus . L.re WT._JString
      >>= \k' -> if k' /= k then Z.rightward cu >>= shuffleToKey else Just cu

    intoElems = WT._JObj . L._1 . L._Wrapped . WT._CommaSeparated . L._2 . L._Just

-- | Move to the first occurence of this key, as per 'moveToKey' and then
-- attempt to run the given 'Decoder' on that value, returning the result.
--
-- @
-- ...
-- txtVal <- fromKey "foo" text c
-- ...
-- @
--
fromKey
  :: ( Monad f
     )
  => Text
  -> Decoder f b
  -> JCursor h Json
  -> DecodeResult f b
fromKey k d =
  moveToKey k >=> runDecoder d

-- | Decoder for 'Scientific'
scientific :: Monad f => Decoder f Scientific
scientific = atCursor "Scientific" DR.scientific'

-- | Decoder for a bounded integral value.
integral :: (Bounded i, Integral i, Monad f) => Decoder f i
integral = atCursor "Integral" DR.integral'

-- | Decoder for 'Int'
int :: Monad f => Decoder f Int
int = atCursor "Int" DR.int'

-- | Decoder for 'Bool'
boolean :: Monad f => Decoder f Bool
boolean = atCursor "Bool" DR.boolean'

-- | Decoder for 'Text', as per the 'Text' documentation any unacceptable utf8 characters will be replaced.
text :: Monad f => Decoder f Text
text = atCursor "Text" DR.text'

-- | Decoder for 'String'
string :: Monad f => Decoder f String
string = atCursor "String" DR.string'

-- | Lean on the 'Cons' and 'AsEmpty' instances from lens to let you provide a
-- 'Decoder' function and collect it into any data structure that has an
-- instance of those typeclasses.
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

-- | 'arrayOfCons' specialised to '[]'.
arrayOf :: Monad f => Decoder f b -> Decoder f [b]
arrayOf d = withCursor (arrayOfCons d)
