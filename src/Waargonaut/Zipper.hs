{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
--
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Zipper where

import qualified GHC.Exts                                  as E

import           Control.Lens                              (Lens', Rewrapped,
                                                            Wrapped (..), iso,
                                                            lens, modifying,
                                                            snoc, to,
                                                            traverseOf, view,
                                                            ( # ), (.~), (^.),
                                                            (^?), _1, _Wrapped)

import           Prelude                                   (Bool, Int,
                                                            fromIntegral, (-),
                                                            (==))

import           Control.Applicative                       (Applicative (..))
import           Control.Category                          ((.))
import           Control.Monad                             (Monad (..), (>=>))

import           Control.Monad.Except                      (liftEither,
                                                            throwError)
import           Control.Monad.State                       (MonadState)

import           Control.Error.Util                        (note)
import           Control.Monad.Error.Hoist                 ((<?>))

import           GHC.Word                                  (Word64)

import           Data.Either                               (Either (..))
import           Data.Foldable                             (fold, foldl)
import           Data.Function                             (($), (&))
import           Data.Functor                              (fmap, (<$), (<$>))
import           Data.Functor.Identity                     (Identity (..))

import           Data.List                                 (replicate)
import           Data.Maybe                                (Maybe (..), maybe)
import           Data.Monoid                               (mempty)
import           Data.Sequence                             ((|>))
import           Data.Traversable                          (traverse)

import           Data.Bool                                 (Bool (..))
import qualified Data.Vector                               as V

import           Data.Scientific                           (toBoundedInteger)
import           Data.Text                                 (Text)

import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8                     as BS8

import           Data.Vector.Storable                      (Vector)
import           Numeric.Natural                           (Natural)

import           Waargonaut.Types
import           Waargonaut.Types.CommaSep

import           HaskellWorks.Data.Positioning             (Count)
import qualified HaskellWorks.Data.Positioning             as Pos

import           HaskellWorks.Data.BalancedParens          (SimpleBalancedParens)
import qualified HaskellWorks.Data.BalancedParens.FindOpen as BP

import           HaskellWorks.Data.Bits                    ((.?.))
import           HaskellWorks.Data.FromByteString          (fromByteString)
import           HaskellWorks.Data.RankSelect.Poppy512     (Poppy512)
import           HaskellWorks.Data.TreeCursor              (TreeCursor (..))

import           HaskellWorks.Data.Json.Succinct.Cursor    (JsonCursor (..))
import qualified HaskellWorks.Data.Json.Succinct.Cursor    as JC

import           Waargonaut.Decode.Internal (CursorHistory',
                                                            DecodeError (..),
                                                            DecodeResultT (..),
                                                            Decoder' (..),
                                                            Mv (..), try,
                                                            withCursor')

type CursorHistory =
  CursorHistory' Count

type SuccinctCursor =
  JsonCursor ByteString Poppy512 (SimpleBalancedParens (Vector Word64))

type DecodeResult f a =
  DecodeResultT Count f DecodeError a

type Decoder f a =
  Decoder' JCurs Count f DecodeError a

newtype JCurs = JCurs
  { unJCurs :: SuccinctCursor
  }

instance JCurs ~ t => Rewrapped JCurs t

instance Wrapped JCurs where
  type Unwrapped JCurs = SuccinctCursor
  _Wrapped' = iso unJCurs JCurs

withCursor
  :: Monad f
  => (JCurs -> DecodeResult f a)
  -> Decoder f a
withCursor =
  withCursor'

mkCursor :: ByteString -> JCurs
mkCursor = JCurs . fromByteString

cursorRankL :: Lens' (JsonCursor s i p) Count
cursorRankL = lens JC.cursorRank (\c r -> c { cursorRank = r })
{-# INLINE cursorRankL #-}

manyMoves :: Monad m => Natural -> (b -> m b) -> b -> m b
manyMoves i g = foldl (>=>) pure (replicate (fromIntegral i) g)

moveCursBasic
  :: Monad f
  => (SuccinctCursor -> Maybe SuccinctCursor)
  -> Mv
  -> JCurs
  -> DecodeResult f JCurs
moveCursBasic f m c =
  traverseOf _Wrapped f c <?> FailedToMove m >>= recordRank m

down
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
down =
  moveCursBasic firstChild D

up
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
up =
  moveCursBasic parent U

moveRightN
  :: Monad f
  => Natural
  -> JCurs
  -> DecodeResult f JCurs
moveRightN i =
  moveCursBasic (manyMoves i nextSibling) (R i)

moveRight1
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
moveRight1 =
  moveRightN 1

moveLeft1
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
moveLeft1 jc =
  let
    c = jc ^. _Wrapped
    rnk = c ^. cursorRankL

    setRank r = jc & _Wrapped . cursorRankL .~ r
    prev = rnk - 1
  in
    setRank <$> BP.findOpen (JC.balancedParens c) prev <?> InputOutOfBounds prev

moveLeftN
  :: Monad f
  => Natural
  -> JCurs
  -> DecodeResult f JCurs
moveLeftN i =
  manyMoves i moveLeft1

jsonAtCursor
  :: ( Monad f
     )
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResult f a
jsonAtCursor p jc = do
  let
    c         = jc ^. _Wrapped
    rnk       = c ^. cursorRankL

    leading   = fromIntegral $ Pos.toCount (JC.jsonCursorPos c)
    cursorTxt = BS8.drop leading (JC.cursorText c)

  if JC.balancedParens c .?. Pos.lastPositionOf rnk
    then liftEither (p cursorTxt)
    else throwError (InputOutOfBounds rnk)

recordRank
  :: ( MonadState CursorHistory f
     , Monad f
     )
  => Mv
  -> JCurs
  -> f JCurs
recordRank mv c =
  c <$ modifying _Wrapped (`snoc` (mv, c ^. _Wrapped . cursorRankL))

bytestringToText
  :: (ByteString -> Either DecodeError JString)
  -> ByteString
  -> Either DecodeError Text
bytestringToText p =
  p >=> note (ConversionFailure "Invalid Text (UTF8 Value)") . (^? _JStringText)

moveToValAtKey
  :: Monad f
  => (ByteString -> Either DecodeError JString)
  -> Text
  -> JCurs
  -> DecodeResult f JCurs
moveToValAtKey p k c =
  -- Tease out the key
  text p c >>= \k' ->
  -- Are we at the key we want to be at ?
  if k' == k
    -- Move into the THING at the key
    then moveRight1 c
    -- Jump to the next key index, the adjacent sibling is opening of the value of the current key
    else moveRightN 2 c >>= moveToValAtKey p k

jtoint :: JNumber -> Either DecodeError Int
jtoint jn = (jNumberToScientific jn >>= toBoundedInteger) <?> ConversionFailure "JNumber out of bounds!"

int
  :: Monad f
  => (ByteString -> Either DecodeError JNumber)
  -> JCurs
  -> DecodeResult f Int
int p =
  jsonAtCursor p >=> liftEither . jtoint

text
  :: Monad f
  => (ByteString -> Either DecodeError JString)
  -> JCurs
  -> DecodeResult f Text
text p =
  jsonAtCursor (bytestringToText p)

-- |
-- Cursor walk version using `nextSibling`
--
arrayByElem
  :: Monad f
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResult f [a]
arrayByElem elemP c = do
  let
    -- <3 Seq.
    consumeElems acc curs = do
      acc' <- (acc |>) <$> jsonAtCursor elemP curs
      try (moveRight1 curs) >>= maybe (pure acc') (consumeElems acc')

  -- Try to move to the first element of the array
  -- Gather the rest of the elements if there are any.
  try (down c) >>= fmap (E.toList . fold) . traverse (consumeElems mempty)

-- |
-- Full array type decoding version
--
array
  :: Monad f
  => (ByteString -> Either DecodeError (JArray ws Json))
  -> (Json -> Either DecodeError a)
  -> JCurs
  -> DecodeResult f [a]
array arrP elemP c = jsonAtCursor arrP c >>=
  liftEither . traverse elemP . view (_Wrapped . to toList)

boolean
  :: Monad f
  => (ByteString -> Either DecodeError Json)
  -> JCurs
  -> DecodeResult f Bool
boolean p c = jsonAtCursor p c >>= \j ->
  j ^? _JBool . _1 <?> ConversionFailure "Expected Boolean"

-- valAtKey
--   :: Monad f
--   => Text
--   -> (JCurs -> DecodeResult f a)
--   -> JCurs
--   -> DecodeResult f a
-- valAtKey k f =
--   moveToValAtKey pJStr k >=> f

-- intoObjAtKey
--   :: Monad f
--   => Text
--   -> JCurs
--   -> DecodeResult f JCurs
-- intoObjAtKey k =
--   moveToValAtKey pJStr k >=> down

-- wutDecoder :: Monad f => Decoder f [Int]
-- wutDecoder = withCursor $ \curs -> do
--   -- Move into the array
--   x <- down curs
--   -- Decode first
--   a <- int pint x
--   -- Step to the right
--   x' <- moveRightN 1 x
--   -- Decode that one
--   b <- int pint x'
--   -- Jump to the left
--   x'' <- moveLeft1 x'
--   -- Decode that one again
--   c <- int pint x''
--   -- Step back to the right again
--   x''' <- moveRightN 1 x''
--   -- Decode that one
--   d <- int pint x'''
--   -- Shake it all out
--   pure [a,b,c,d]

-- fooDecoder :: Monad f => Decoder f [Int]
-- fooDecoder = withCursor $ \curs -> do
--   -- Move into array
--   x <- down curs
--   -- Decode '1'
--   a <- int pint x
--   -- Step right twice
--   x' <- moveRightN 2 x
--   -- Decode '3'
--   b <- int pint x'
--   -- Step left once
--   x'' <- moveLeftN 1 x'
--   -- Decode '2'
--   c <- int pint x''
--   -- Step right once
--   x''' <- moveRightN 1 x''
--   -- Decode '3'
--   d <- int pint x'''
--   -- Step left twice
--   x'''' <- moveLeftN 2 x'''
--   -- Decode '1'
--   e <- int pint x''''
--   -- Expecting [1,3,2,3,1]
--   pure [a,b,c,d,e]

-- encodeImage :: Encoder Image
-- encodeImage = encodeAsMapLike $ \img ->
--   intAt "Width" (_imageW img) .
--   intAt "Height" (_imageH img) .
--   textAt "Title" (_imageTitle img) .
--   boolAt "Animated" (_imageAnimated img) .
--   arrayAt encodeInt "IDs" (_imageIDs img)

-- kv :: (At s, IxValue s ~ Json) => Encoder a -> Index s -> a -> h :>> s -> h :>> s
-- kv enc k v z = z & Z.downward (at k) & Z.focus ?~ runEncoder enc v & Z.upward

-- intAt :: Text -> Int -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- intAt = kv encodeInt

-- textAt :: Text -> Text -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- textAt = kv encodeText

-- boolAt :: Text -> Bool -> h :>> MapLikeObj WS Json -> h :>> MapLikeObj WS Json
-- boolAt = kv encodeBool

-- arrayAt :: (At s, Foldable f, IxValue s ~ Json) => Encoder a -> Index s -> f a -> h :>> s -> h :>> s
-- arrayAt enc = kv (encodeArray enc)

-- obj :: (Z.Zipped h a ~ MapLikeObj WS Json, Z.Zipping h a) => Zipper h i a -> Json
-- obj o = _JObj # (fromMapLikeObj $ Z.rezip o, mempty)

-- encodeImageZip :: Image -> Z.Top :>> MapLikeObj WS Json
-- encodeImageZip i = mapLikeObj
--   & intAt "Width" (_imageW i)
--   & intAt "Height" (_imageH i)
--   & textAt "Title" (_imageTitle i)
--   & boolAt "Animated" (_imageAnimated i)
--   & arrayAt encodeInt "IDs" (_imageIDs i)

-- encodeSomething :: Image -> [Int] -> Json
-- encodeSomething i xs = mapLikeObj
--   & Z.downward (at "image") & Z.focus ?~ obj (encodeImageZip i) & Z.upward
--   & Z.downward (at "ints") & Z.focus ?~ runEncoder (encodeArray encodeInt) xs
--   & obj

jboolFalse :: Json
jboolFalse = Json (JBool False mempty)

jboolTrue :: Json
jboolTrue = Json (JBool True mempty)

-- -- {"abc":false}
obj :: Json
obj = Json (JObj (JObject cs) mempty)
  where
    js = _JStringText # "abc"
    js2 = _JStringText # "def"

    cs = CommaSeparated mempty (
      Just (
          Elems
            (V.singleton (Elem (JAssoc js mempty mempty jboolFalse) (Identity (Comma, WS mempty))))
            (Elem (JAssoc js2 mempty mempty jboolTrue) Nothing)
          )
      )
