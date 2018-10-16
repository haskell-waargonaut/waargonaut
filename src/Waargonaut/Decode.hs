{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- | Types and functions to convert Json values into your data types.
module Waargonaut.Decode
  (
    Err
  , CursorHistory (..)
  , DecodeResult (..)

    -- * Type aliases
  , JCursorMove
  , JCursor
  , Decoder

    -- * Decoder creation
  , withCursor

    -- * Decoder execution
  , runDecoder
  , runDecoderResult
  , runPureDecode
  , simpleDecode
  , generaliseDecoder

    -- * Cursor movement
  , into
  , up
  , down
  , moveLeftN
  , moveLeft1
  , moveRightN
  , moveRight1
  , moveToKey
  , try

    -- * Decode at Cursor
  , fromKey
  , atKey
  , atCursor
  , focus

    -- * Provided decoders
  , scientific
  , integral
  , int
  , bool
  , text
  , string
  , boundedChar
  , unboundedChar
  , null
  , json
  , foldCursor
  , leftwardCons
  , rightwardSnoc
  , nonEmptyAt
  , nonempty
  , listAt
  , list
  , maybeOrNull
  , withDefault
  , either

  ) where

import           Prelude                       hiding (either, maybe, null)

import           Numeric.Natural               (Natural)

import           Control.Lens                  (Bazaar', Cons, LensLike', Snoc,
                                                (^.), (^?))
import qualified Control.Lens                  as L

import           Control.Lens.Internal.Indexed (Indexed, Indexing)
import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (MonadError)
import           Control.Monad.Morph           (MFunctor (..), MMonad (..),
                                                generalize)
import           Control.Monad.State           (MonadState)
import           Control.Monad.Trans.Class     (MonadTrans (..))

import           Control.Error.Util            (note)
import           Control.Monad.Error.Hoist     ((<%?>), (<?>))

import           Control.Zipper                ((:>>))
import qualified Control.Zipper                as Z

import           Data.Functor.Identity         (Identity, runIdentity)
import qualified Data.Maybe                    as Maybe

import           Data.List.NonEmpty            (NonEmpty ((:|)))

import qualified Data.Bool                     as Bool
import           Data.Text                     (Text)

import           Data.Scientific               (Scientific)

import           Waargonaut.Types              (AsJType, Elems, JAssoc, Json)

import qualified Waargonaut.Types              as WT

import           Waargonaut.Decode.Error       (Err' (..))
import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeError (..), DecodeResultT,
                                                Decoder' (..), ZipperMove (..),
                                                runDecoderResultT, try)

import qualified Waargonaut.Decode.Internal    as DR

-- | Convenience Error structure for the separate parsing/decoding phases. For
-- when things really aren't that complicated.
type Err e = Err' CursorHistory e

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
  { unDecodeResult :: DecodeResultT Int DecodeError f a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (CursorHistory' Int)
           , MonadError DecodeError
           )

instance MonadTrans DecodeResult where
  lift = DecodeResult . lift

instance MFunctor DecodeResult where
  hoist nat (DecodeResult dr) = DecodeResult (hoist nat dr)

instance MMonad DecodeResult where
  embed f (DecodeResult dr) = DecodeResult (embed (unDecodeResult . f) dr)

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
  forall h. Decoder' (JCursor h Json) Int DecodeError f a

-- | Generalise a 'Decoder' that has been specialised to 'Identity' back to some 'Monad f'.
generaliseDecoder :: Monad f => Decoder Identity a -> Decoder f a
generaliseDecoder dr = Decoder' (embed generalize . runDecoder' dr)
{-# INLINE generaliseDecoder #-}

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
--   \<*> D.fromKey \"Animated\" D.bool curs
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

-- |
-- Run a pure decoder with 'Identity'.
--
runPureDecode
  :: Decoder Identity a
  -> JCursor h Json
  -> Either (DecodeError, CursorHistory) a
runPureDecode dec = runIdentity
  . runDecoderResult
  . runDecoder dec

-- |
-- Using the given parsing function, take some input and try to convert it to
-- the 'Json' structure. Then pass it to the given 'Decoder'.
simpleDecode
  :: (s -> Either e Json)
  -> Decoder Identity a
  -> s
  -> Either (Err e) a
simpleDecode p dec =
  L.bimap Parse Z.zipper . p >=>
  L.over L._Left Decode . runPureDecode dec

-- Helper function that takes a given attempt to move the cursor to a new
-- location, throwing the 'FailedToMove' error if it fails, or recording the new
-- position and returning the new position.
moveAndKeepHistory
  :: Monad f
  => ZipperMove
  -> Maybe (JCursor h s)
  -> DecodeResult f (JCursor h s)
moveAndKeepHistory dir mCurs = do
  a <- mCurs <?> FailedToMove dir
  a <$ DR.recordZipperMove dir (Z.tooth a)

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

-- | A constrained version of 'into' that will only move a single step down into
-- a 'Json' value. The 'Text' input is so you're able to provide an expectation
-- of what you are stepping down into, this provides a more descriptive error
-- message than simply "down".
--
-- For example:
--
-- @
-- firstElemCursor <- down "array" cursor
-- @
--
down
  :: Monad f
  => Text
  -> JCursor h Json
  -> DecodeResult f (JCursor (JCursor h Json) Json)
down tgt =
  into tgt WT.jsonTraversal

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
  b <$ DR.recordZipperMove (Item t) (Z.tooth c)

-- | From the current cursor position, try to move to the value for the first
-- occurence of that key. This move expects that you've positioned the cursor on an
-- object.
moveToKey
  :: ( AsJType s ws s
     , Monad f
     )
  => Text
  -> JCursor h s
  -> DecodeResult f (h :>> s :>> Elems ws (JAssoc ws s) :>> JAssoc ws s :>> s)
moveToKey k =
  moveAndKeepHistory (DAt k)
  . ( Z.within intoElems
      >=> Z.within traverse
      >=> shuffleToKey
      >=> Z.within WT.jsonAssocVal
    )
  where
    shuffleToKey cu = Z.within WT.jsonAssocKey cu ^? L._Just . Z.focus . L.re WT._JString
      >>= Bool.bool (Just cu) (Z.rightward cu >>= shuffleToKey) . (/=k)

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

-- | A simplified version of 'fromKey' that takes a 'Text' value indicating a
-- key to be moved to and decoded using the given 'Decoder f a'. If you don't
-- need any special cursor movements to reach the list of keys you require, you
-- could use this function to build a trivial 'Decoder' for a record type:
--
-- @
-- data MyRec = MyRec { fieldA :: Text, fieldB :: Int }
--
-- myRecDecoder :: Decoder f MyRec
-- myRecDecoder = MyRec
--   <$> atKey "field_a" text
--   <*> atKey "field_b" int
-- @
--
atKey
  :: Monad f
  => Text
  -> Decoder f a
  -> Decoder f a
atKey k d =
  withCursor (fromKey k d)

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
bool :: Monad f => Decoder f Bool
bool = atCursor "Bool" DR.bool'

-- | Decoder for 'Text', as per the 'Text' documentation any unacceptable utf8 characters will be replaced.
text :: Monad f => Decoder f Text
text = atCursor "Text" DR.text'

-- | Decoder for 'String'
string :: Monad f => Decoder f String
string = atCursor "String" DR.string'

-- | Decoder for 'null'
null :: Monad f => Decoder f ()
null = atCursor "null" DR.null'

-- | Decoder for a 'Char' value that cannot contain values in the range U+D800
-- to U+DFFF. This decoder will fail if the 'Char' is outside of this range.
boundedChar :: Monad f => Decoder f Char
boundedChar = atCursor "Bounded Char" DR.boundedChar'

-- | Decoder for a Haskell 'Char' value whose values represent Unicode
-- (or equivalently ISO/IEC 10646) characters.
unboundedChar :: Monad f => Decoder f Char
unboundedChar = atCursor "Unbounded Char" DR.unboundedChar'

-- | Decoder for pulling out the 'Json' Haskell data structure at the current cursor.
json :: Monad f => Decoder f Json
json = atCursor "JSON" pure

-- | Try to decode the value at the current focus using the given 'Decoder'.
focus
  :: Monad f
  => Decoder f a
  -> JCursor h Json
  -> DecodeResult f a
focus =
  runDecoder

-- | Allows for folding over the results of repeated cursor movements.
--
-- @
-- intList :: Decoder f [String]
-- intList = withCursor $ \curs ->
--   foldCursor [] (\acc a -> acc <> [a]) moveRight1 string curs
-- @
--
foldCursor
  :: Monad f
  => s
  -> (s -> a -> s)
  -> (JCursor h Json -> DecodeResult f (JCursor h Json))
  -> Decoder f a
  -> JCursor h Json
  -> DecodeResult f s
foldCursor s sas mvCurs elemD = DecodeResult
  . DR.foldCursor'
    s
    sas
    (unDecodeResult . mvCurs)
    elemD

-- | Use the 'Cons' typeclass and move leftwards from the current cursor
-- position, 'consing' the values to the 's' as it moves.
leftwardCons
  :: ( Monad f
     , Cons s s a a
     )
  => s
  -> Decoder f a
  -> JCursor h Json
  -> DecodeResult f s
leftwardCons s elemD = DecodeResult
  . DR.foldCursor' s
    (flip L.cons)
    (unDecodeResult . moveLeft1)
    elemD

-- | Use the 'Snoc' typeclass and move rightwards from the current cursor
-- position, 'snocing' the values to the 's' as it moves.
rightwardSnoc
  :: ( Monad f
     , Snoc s s a a
     )
  => s
  -> Decoder f a
  -> JCursor h Json
  -> DecodeResult f s
rightwardSnoc s elemD = DecodeResult
  . DR.foldCursor' s
    L.snoc
    (unDecodeResult . moveRight1)
    elemD

-- | Decode a 'NonEmpty' list of 'a' at the given cursor position.
nonEmptyAt
  :: Monad f
  => Decoder f a
  -> JCursor h Json
  -> DecodeResult f (NonEmpty a)
nonEmptyAt elemD c =
  moveAndKeepHistory D (Z.within WT.jsonTraversal c)
  >>= \curs -> do
    h <- focus elemD curs
    moveRight1 curs >>= fmap (h:|) . rightwardSnoc [] elemD

-- | Create a 'Decoder' for a 'NonEmpty' list.
nonempty :: Monad f => Decoder f b -> Decoder f (NonEmpty b)
nonempty d = withCursor (nonEmptyAt d)

-- | Decode a '[a]' at the current cursor position.
listAt
  :: Monad f
  => Decoder f a
  -> JCursor h Json
  -> DecodeResult f [a]
listAt elemD c =
  try (moveAndKeepHistory D (Z.within WT.jsonTraversal c))
  >>= Maybe.maybe (pure mempty) (rightwardSnoc mempty elemD)

-- | Create a 'Decoder' for a list of 'a'
list
  :: Monad f
  => Decoder f b
  -> Decoder f [b]
list d =
  withCursor (listAt d)

-- | Try to decode an optional value, returning the given default value if
-- 'Nothing' is returned.
withDefault
  :: Monad f
  => a
  -> Decoder f (Maybe a)
  -> Decoder f a
withDefault def hasD =
  withCursor (fmap (Maybe.fromMaybe def) . focus hasD)

-- | Named to match it's 'Encoder' counterpart, this function will decode an
-- optional value.
maybeOrNull
  :: Monad f
  => Decoder f a
  -> Decoder f (Maybe a)
maybeOrNull hasD =
  withCursor (try . focus hasD)

-- | Decode either an 'a' or a 'b', failing if neither 'Decoder' succeeds. The
-- 'Right' decoder is attempted first.
either
  :: Monad f
  => Decoder f a
  -> Decoder f b
  -> Decoder f (Either a b)
either leftD rightD =
  withCursor $ \c ->
    try (focus (Right <$> rightD) c) >>=
    Maybe.maybe (focus (Left <$> leftD) c) pure
