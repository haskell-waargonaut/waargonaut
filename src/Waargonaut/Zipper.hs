{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
--
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Waargonaut.Zipper where

import qualified GHC.Exts                               as E

import           Control.Lens                           (Lens', lens,
                                                         makeWrapped, modifying,
                                                         preview, snoc, to, re,
                                                         traverseOf, view, (.=), (#), (^..),
                                                         (.~), (^.), (^?), _1,
                                                         _2, _Snoc, _Wrapped,
                                                         _last)
import           Prelude                                (Bool, Eq, IO, Int,
                                                         Show, fromIntegral,
                                                         print, (==), (<), abs)

import           Control.Applicative                    (Applicative (..), liftA2)
import           Control.Category                       ((.))
import           Control.Monad                          (Monad (..), (<=<),
                                                         (>=>))

import           Control.Monad.Except                   (ExceptT, MonadError,
                                                         liftEither, runExceptT,
                                                         throwError)
import           Control.Monad.Reader                   (MonadReader,
                                                         ReaderT (..),
                                                         runReaderT)
import           Control.Monad.State                    (MonadState, StateT,
                                                         gets, runStateT)

import           Control.Error.Util                     (note)
import           Control.Monad.Error.Hoist              ((<?>))

import           GHC.Word                               (Word64)

import Data.List.NonEmpty (NonEmpty (..))
import           Data.Bifunctor                         (first)
import           Data.Either                            (Either (..))
import           Data.Foldable                          (Foldable,fold,foldr)
import           Data.Function                          (flip, ($), (&))
import           Data.Functor                           (Functor, fmap, (<$),
                                                         (<$>))
import           Data.Functor.Identity                  (Identity (..),
                                                         runIdentity)
import           Data.Maybe                             (Maybe (..), maybe)
import           Data.Monoid                            (mempty)
import           Data.Sequence                          (Seq, (|>))
import           Data.Traversable                       (traverse)

import           Data.Scientific                        (toBoundedInteger)
import           Data.Text                              (Text)

import           Data.ByteString.Char8                  (ByteString)
import qualified Data.ByteString.Char8                  as BS8

import           Data.Vector.Storable                   (Vector)

import qualified Data.Vector                            as V

import           Data.Digit                             (Digit, HeXaDeCiMaL)
import qualified Data.Digit as D

import           Text.ParserCombinators.ReadP           (ReadP)
import qualified Text.ParserCombinators.ReadP           as RP

import           Waargonaut.Types
import           Waargonaut.Types.CommaSep

import           HaskellWorks.Data.Positioning          (Count)
import qualified HaskellWorks.Data.Positioning          as Pos

import           HaskellWorks.Data.BalancedParens       (SimpleBalancedParens)
import           HaskellWorks.Data.Bits                 ((.?.))
import           HaskellWorks.Data.FromByteString       (fromByteString)
import           HaskellWorks.Data.RankSelect.Poppy512  (Poppy512)
import           HaskellWorks.Data.TreeCursor           (TreeCursor (..))

import           HaskellWorks.Data.Json.Succinct.Cursor (JsonCursor (..))
import qualified HaskellWorks.Data.Json.Succinct.Cursor as JC

data DecodeError
  = ConversionFailure Text
  | KeyDecodeFailed Text
  | FailedToMove
  | IntOutOfBounds
  | InputOutOfBounds Word64
  | ParseFailed Text
  deriving (Show, Eq)

data CursorMove
  = FirstChild
  | NextSibling
  | Parent
  deriving (Show, Eq)

newtype CursorHistory = CursorHistory (Seq (CursorMove, Count))
  deriving (Eq, Show)
makeWrapped ''CursorHistory

type SuccinctCursor =
  JsonCursor ByteString Poppy512 (SimpleBalancedParens (Vector Word64))

newtype JCurs = JCurs
  { unJCurs :: SuccinctCursor
  }
makeWrapped ''JCurs

newtype DecodeResultT f e a = DecodeResultT
  { runDecodeResult :: ExceptT e (StateT CursorHistory f) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState CursorHistory
           , MonadError e
           )

runDecoderResultT
  :: Monad f
  => DecodeResultT f DecodeError a
  -> f (Either (DecodeError, CursorHistory) a)
runDecoderResultT =
  fmap (\(e, hist) -> first (,hist) e)
  . flip runStateT (CursorHistory mempty)
  . runExceptT
  . runDecodeResult

newtype Decoder f a = Decoder
  { unDecoder :: ReaderT JCurs (DecodeResultT f DecodeError) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader JCurs
           )

newtype Encoder a = Encoder
  { unEncoder :: a -> Json
  }

encodeA
  :: (a -> Json)
  -> Encoder a
encodeA =
  Encoder

encodeInt :: Encoder Int
encodeInt = encodeA $ \i -> _JNum # (JNumber (i < 0) (mkjInt $ abs i) Nothing Nothing, mempty)
  where
    mkjInt :: Int -> JInt
    mkjInt 0 = JZero
    mkjInt n = (\(h :| t) -> JIntInt h t) $ D._NaturalDigits # fromIntegral n

encodeBool :: Encoder Bool
encodeBool = encodeA ((_JBool #) . (,mempty))

withCursor
  :: Monad f
  => (JCurs -> DecodeResultT f DecodeError a)
  -> Decoder f a
withCursor =
  Decoder . ReaderT

runDecoder
  :: Monad f
  => Decoder f a
  -> JCurs
  -> DecodeResultT f DecodeError a
runDecoder d =
  runReaderT (unDecoder d)

mkCursor :: ByteString -> JCurs
mkCursor = JCurs . fromByteString

cursorRankL :: Lens' (JsonCursor s i p) Count
cursorRankL = lens JC.cursorRank (\c r -> c { cursorRank = r })
{-# INLINE cursorRankL #-}

jsonAtCursor
  :: ( Monad f
     )
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f DecodeError a
jsonAtCursor p jc = do
  let
    c         = jc ^. _Wrapped
    rnk       = c ^. cursorRankL
    leading   = fromIntegral $ Pos.toCount (JC.jsonCursorPos c)
    cursorTxt = BS8.drop leading (JC.cursorText c)

  if JC.balancedParens c .?. Pos.lastPositionOf rnk
    then liftEither (p cursorTxt)
    else throwError (InputOutOfBounds rnk)

moveFn :: CursorMove -> (SuccinctCursor -> Maybe SuccinctCursor)
moveFn FirstChild  = firstChild
moveFn NextSibling = nextSibling
moveFn Parent      = parent

recordRank
  :: ( MonadState CursorHistory f
     , Monad f
     )
  => CursorMove
  -> JCurs
  -> f JCurs
recordRank mv c =
  c <$ modifying _Wrapped (`snoc` (mv, c ^. _Wrapped . cursorRankL))

tryMoveJCurs
  :: Monad f
  => CursorMove
  -> JCurs
  -> DecodeResultT f e (Maybe JCurs)
tryMoveJCurs mv =
  traverse (recordRank mv) . traverseOf _Wrapped (moveFn mv)

moveJCurs
  :: Monad f
  => CursorMove
  -> JCurs
  -> DecodeResultT f DecodeError JCurs
moveJCurs mv c =
  traverseOf _Wrapped (moveFn mv) c <?> FailedToMove >>= recordRank mv

moveToValAtKey
  :: ( Monad f
     , HeXaDeCiMaL digit
     )
  => (ByteString -> Either DecodeError (JString digit))
  -> Text
  -> JCurs
  -> DecodeResultT f DecodeError JCurs
moveToValAtKey p k c =
  -- Tease out the key
  jsonAtCursor (fmap jStringToText . p) c >>= \k' ->
  -- Are we at the key we want to be at ?
  if k' == k
    -- Move into the THING at the key
    then moveJCurs NextSibling c
    -- Jump to the next key index, the adjacent sibling is opening of the value of the current key
    else moveJCurs NextSibling c
         >>= moveJCurs NextSibling
         >>= moveToValAtKey p k

back
  :: Monad f
  => JCurs
  -> DecodeResultT f e (Maybe JCurs)
back c =
  -- See if we can pull off the last rank we were at.
  gets (^? _Wrapped . _Snoc) >>=
  -- If we can then, set the state to be the snocced list and the current rank to be the previous rank.
  traverse (\(ps, p) -> (c & _Wrapped . cursorRankL .~ (p ^. _2)) <$ (_Wrapped .= ps))

jtoint :: JNumber -> Either DecodeError Int
jtoint jn = (jNumberToScientific jn >>= toBoundedInteger) <?> ConversionFailure "Number out of bounds!"

int
  :: Monad f
  => (ByteString -> Either DecodeError JNumber)
  -> JCurs
  -> DecodeResultT f DecodeError Int
int p =
  jsonAtCursor p >=> liftEither . jtoint

text
  :: ( HeXaDeCiMaL digit
     , Monad f
     )
  => (ByteString -> Either DecodeError (JString digit))
  -> JCurs
  -> DecodeResultT f DecodeError Text
text p c =
  jStringToText <$> jsonAtCursor p c

-- |
-- Cursor walk version using `nextSibling`
--
arrayWithUnfoldr
  :: Monad f
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f DecodeError [a]
arrayWithUnfoldr elemP c = do
  let
    tryElemP = traverse (jsonAtCursor elemP)

    liftAA2 = liftA2 . liftA2
    moosh h = V.toList . V.cons h

    f c' = do
      e <- tryMoveJCurs NextSibling c'
      j <- tryElemP e
      pure $ liftA2 (,) j e

  -- unfoldrM edition, not sure if better. Seems pretty wooly and a bit silly.
  tryMoveJCurs FirstChild c >>= \hc ->
    fold <$> liftAA2 moosh (tryElemP hc) (traverse (V.unfoldrM f) hc)

arrayWithSeq
  :: Monad f
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f DecodeError [a]
arrayWithSeq elemP c = do
  let
    -- Sean <3 Seq.
    consumeElems acc curs = do
      acc' <- (acc |>) <$> jsonAtCursor elemP curs
      tryMoveJCurs NextSibling curs
        >>= maybe (pure acc') (consumeElems acc')

  -- Try to move to the first element of the array
  tryMoveJCurs FirstChild c >>=
    -- Gather the rest of the elements if there are any.
    fmap (E.toList . fold) . traverse (consumeElems mempty)

-- |
-- Full array type decoding version
--
array
  :: Monad f
  => (ByteString -> Either DecodeError (JArray ws Json))
  -> (Json -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f DecodeError [a]
array arrP elemP c = jsonAtCursor arrP c >>=
  liftEither . traverse elemP . view (_Wrapped . to toList)

boolean
  :: Monad f
  => (ByteString -> Either DecodeError Json)
  -> JCurs
  -> DecodeResultT f DecodeError Bool
boolean p c = jsonAtCursor p c >>= \j ->
  j ^? _JBool . _1 <?> ConversionFailure "Expected Boolean"

data Image = Image
  { _imageW        :: Int
  , _imageH        :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
  deriving Show

parsur
  :: Show a
  => ReadP a
  -> Text
  -> ByteString
  -> Either DecodeError a
parsur p t = note (ParseFailed t)
  -- readP will give us EVERYTHING that the parser allows, this is wild.
  . preview (_last . _1)
  . RP.readP_to_S p
  . BS8.unpack

pJStr
  :: ByteString
  -> Either DecodeError (JString Digit)
pJStr = parsur parseJString "JString"

pint
  :: ByteString
  -> Either DecodeError JNumber
pint = parsur parseJNumber "JNumber"

poolean
  :: ByteString
  -> Either DecodeError Json
poolean = parsur (Json <$> parseJBool parseWhitespace) "JBool"

parray
  :: ByteString
  -> Either DecodeError (JArray WS Json)
parray = parsur (parseJArray parseWhitespace parseWaargonaut) "JArray"

pjnum
  :: Json
  -> Either DecodeError Int
pjnum = (jtoint <=< note (ConversionFailure "Expected JNumber")) . preview (_JNum . _1)

down
  :: Monad f
  => JCurs
  -> DecodeResultT f DecodeError JCurs
down =
  moveJCurs FirstChild

valAtKey
  :: Monad f
  => Text
  -> (JCurs -> DecodeResultT f DecodeError a)
  -> JCurs
  -> DecodeResultT f DecodeError a
valAtKey k f =
  moveToValAtKey pJStr k >=> f

intoObjAtKey
  :: Monad f
  => Text
  -> JCurs
  -> DecodeResultT f DecodeError JCurs
intoObjAtKey k =
  moveToValAtKey pJStr k >=> down

imageDecoder :: Monad f => Decoder f Image
imageDecoder = withCursor $ \curs -> do
  -- We're at the root of our object, move into it and move to the value at the
  -- "Image" key
  c <- down curs >>= intoObjAtKey "Image"
  -- We need individual values off of our object,
  Image
    <$> valAtKey "Width" (int pint) c
    <*> valAtKey "Height" (int pint) c
    <*> valAtKey "Title" (text pJStr) c
    <*> valAtKey "Animated" (boolean poolean) c
    -- <*> valAtKey "IDs" (array parray pjnum) c
    <*> valAtKey "IDs" (arrayWithUnfoldr (pint >=> jtoint)) c

-- |
-- A filthy test implementation of my filthy decoders and their respective
-- filthy parsers being wired in for the purposes of decoding from a JSON
-- bytestring into an actual Haskell type.
--
decodeTest1Json :: IO ()
decodeTest1Json = do
  cur <- mkCursor <$> BS8.readFile "test/json-data/test1.json"
  print . runIdentity . runDecoderResultT $ runDecoder imageDecoder cur

-- jboolFalse :: Json
-- jboolFalse = Json (JBool False mempty)

-- jboolTrue :: Json
-- jboolTrue = Json (JBool True mempty)

-- -- {"abc":false}
-- obj :: Json
-- obj = Json (JObj (JObject cs) mempty)
--   where
--     js = JString $ V.fromList
--       [ UnescapedJChar (JCharUnescaped 'a')
--       , UnescapedJChar (JCharUnescaped 'b')
--       , UnescapedJChar (JCharUnescaped 'c')
--       ]

--     cs = CommaSeparated mempty (
--       Just (
--           Elems
--             (V.singleton (Elem (JAssoc js mempty mempty jboolFalse) (Identity (Comma, WS mempty))))
--             (Elem (JAssoc js mempty mempty jboolTrue) Nothing)
--           )
--       )

-- data Dir
--   = Lft
--   | Rgt
--   | Up
--   | Into Text
--   | IntoIdx Int
--   deriving (Show, Eq)

-- type JCursor h a =
--   h :>> a

-- type JCursorMove s a =
--   LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a

-- newtype DecodeResult a = DecodeResult
--   { unDecodeResult :: ReaderT Json (ExceptT DecodeError (State CursorHistory)) a
--   }
--   deriving ( Functor
--            , Applicative
--            , Monad
--            , MonadReader Json
--            , MonadError DecodeError
--            , MonadState CursorHistory
--            )

-- moveAndKeepHistory
--   :: Dir
--   -> Maybe (JCursor h s)
--   -> DecodeResult (JCursor h s)
-- moveAndKeepHistory dir mCurs = do
--   a <- either throwError pure . note (FailedToMove dir) $ mCurs
--   modify (\(CursorHistory ch) -> CursorHistory $ snoc ch dir)
--   pure a

-- newCursor
--   :: DecodeResult (JCursor Top Json)
-- newCursor =
--   zipper <$> ask

-- into
--   :: Text
--   -> JCursorMove s a
--   -> JCursor h s
--   -> DecodeResult (JCursor (JCursor h s) a)
-- into tgt l =
--   moveAndKeepHistory (Into tgt) . withins l

-- up
--   :: JCursor (JCursor h s) a
--   -> DecodeResult (JCursor h s)
-- up =
--   moveAndKeepHistory Up . pure . upward

-- stepLeft
--   :: JCursor h a
--   -> DecodeResult (JCursor h a)
-- stepLeft =
--   moveAndKeepHistory Lft . leftward

-- stepRight
--   :: JCursor h a
--   -> DecodeResult (JCursor h a)
-- stepRight =
--   moveAndKeepHistory Rgt . rightward

-- withCursor
--   :: (a -> Either Text b)
--   -> JCursor h a
--   -> DecodeResult b
-- withCursor f =
--   either (throwError . ConversionFailure) pure . f . view focus

-- integral
--   :: ( Bounded i
--      , Integral i
--      )
--   => JCursor h JNumber
--   -> DecodeResult i
-- integral = either throwError pure
--   . note IntOutOfBounds
--   . (toBoundedInteger <=< jNumberToScientific)
--   . view focus

-- runDecode
--   :: Json
--   -> DecodeResult a
--   -> Either (CursorHistory, DecodeError) a
-- runDecode j =
--   let
--     f = flip runState (CursorHistory mempty)
--       . runExceptT
--       . flip runReaderT j
--       . unDecodeResult

--     g (r, z) =
--       over _Left (z,) r
--   in
--     g . f
