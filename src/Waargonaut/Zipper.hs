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

import           Control.Lens                           (Lens', lens,
                                                         makeWrapped, modifying,
                                                         over, preview, snoc,
                                                         to, traverseOf, view,
                                                         (.=), (.~), (^.), (^?),
                                                         _1, _Left, _Snoc,
                                                         _Wrapped, _last)
import           Prelude                                (Bool, Eq, IO, Int,
                                                         Show, fromIntegral,
                                                         print, (==))

import           Control.Applicative                    (Applicative, pure)
import           Control.Category                       ((.))
import           Control.Monad                          (Monad, (<=<), (=<<),
                                                         (>=>), (>>=))

import           Control.Monad.Except                   (ExceptT, MonadError,
                                                         runExceptT, throwError)
import           Control.Monad.State                    (MonadState, StateT,
                                                         gets, runStateT)

import           Control.Error.Util                     (note)

import           GHC.Word                               (Word64)

import           Data.Either                            (Either (..), either)
import           Data.Maybe                             (Maybe)
import           Data.Monoid                            (mempty)
import           Data.Sequence                          (Seq)

import           Data.Function                          (flip, ($), (&))
import           Data.Functor                           (Functor, fmap, (<$),
                                                         (<$>))
import           Data.Functor.Identity                  (runIdentity)
import           Data.Traversable                       (traverse)

import           Data.Scientific                        (toBoundedInteger)
import           Data.Text                              (Text)

import           Data.ByteString.Char8                  (ByteString)
import qualified Data.ByteString.Char8                  as BS8

import           Data.Vector.Storable                   (Vector)

import           Data.Digit                             (Digit, HeXaDeCiMaL)

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

newtype CursorHistory =
  CursorHistory (Seq Count)
  deriving (Eq, Show)
makeWrapped ''CursorHistory

type SuccinctCursor =
  JsonCursor ByteString Poppy512 (SimpleBalancedParens (Vector Word64))

newtype JCurs = JCurs
  { unJCurs :: SuccinctCursor
  }
makeWrapped ''JCurs

newtype DecodeResultT f a = DecodeResultT
  { runDecodeResult :: ExceptT DecodeError (StateT CursorHistory f) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState CursorHistory
           , MonadError DecodeError
           )

newtype Decoder f a = Decoder (JCurs -> DecodeResultT f a)
  deriving Functor

runDecoder
  :: Monad f
  => DecodeResultT f a
  -> f (Either (DecodeError, CursorHistory) a)
runDecoder =
  fmap (\(e, hist) -> over _Left (,hist) e)
  . flip runStateT (CursorHistory mempty)
  . runExceptT
  . runDecodeResult

mkCursor :: ByteString -> JCurs
mkCursor = JCurs . fromByteString

cursorRankL :: Lens' (JsonCursor s i p) Count
cursorRankL = lens JC.cursorRank (\c r -> c { cursorRank = r })
{-# INLINE cursorRankL #-}

handleErr
  :: MonadError e m
  => Either e a
  -> m a
handleErr =
  either throwError pure
{-# INLINE handleErr #-}

jsonAtCursor
  :: ( Monad f
     )
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f a
jsonAtCursor p jc = do
  let
    c = jc ^. _Wrapped
    rnk = c ^. cursorRankL
    lead = fromIntegral $ Pos.toCount (JC.jsonCursorPos c)
    cursorTxt = BS8.drop lead (JC.cursorText c)

  if JC.balancedParens c .?. Pos.lastPositionOf rnk
    then handleErr (p cursorTxt)
    else throwError (InputOutOfBounds rnk)

moveJCurs
  :: Monad f
  => (SuccinctCursor -> Maybe SuccinctCursor)
  -> JCurs
  -> DecodeResultT f JCurs
moveJCurs mv c = do
  c' <- handleErr . note FailedToMove $ traverseOf _Wrapped mv c
  c' <$ modifying _Wrapped (`snoc` c' ^. _Wrapped . cursorRankL)

moveToValAtKey
  :: ( Monad f
     , HeXaDeCiMaL digit
     )
  => (ByteString -> Either DecodeError (JString digit))
  -> Text
  -> JCurs
  -> DecodeResultT f JCurs
moveToValAtKey p k c = do
  -- Tease out the key
  k' <- jsonAtCursor (fmap jStringToText . p) c
  -- Are we at the key we want to be at ?
  if k' == k
    -- Move into the THING at the key
    then moveJCurs nextSibling c
    -- Jump to the next key index, the adjacent sibling is opening of the value of the current key
    else moveJCurs nextSibling c >>= moveJCurs nextSibling >>= moveToValAtKey p k

back
  :: Monad f
  => JCurs
  -> DecodeResultT f (Maybe JCurs)
back c = do
  -- See if we can pull off the last rank we were at.
  mSnocced <- gets (^? _Wrapped . _Snoc)
  -- If we can then set the state to be snocced list and the current rank to be the previous last of the history.
  traverse (\(ps, p) -> (c & _Wrapped . cursorRankL .~ p) <$ (_Wrapped .= ps)) mSnocced

jtoint :: JNumber -> Either DecodeError Int
jtoint = note (ConversionFailure "Number out of bounds!") . (toBoundedInteger <=< jNumberToScientific)

int
  :: Monad f
  => (ByteString -> Either DecodeError JNumber)
  -> JCurs
  -> DecodeResultT f Int
int p = jsonAtCursor p >=> handleErr . jtoint

text
  :: ( HeXaDeCiMaL digit
     , Monad f
     )
  => (ByteString -> Either DecodeError (JString digit))
  -> JCurs
  -> DecodeResultT f Text
text p c = jStringToText <$> jsonAtCursor p c

array
  :: Monad f
  => (ByteString -> Either DecodeError (JArray ws Json))
  -> (Json -> Either DecodeError a)
  -> JCurs
  -> DecodeResultT f [a]
array arrP elemP c = jsonAtCursor arrP c >>= handleErr
  . traverse elemP
  . view (_Wrapped . to toList)

boolean
  :: Monad f
  => (ByteString -> Either DecodeError Json)
  -> JCurs
  -> DecodeResultT f Bool
boolean p c = jsonAtCursor p c >>= handleErr
  . note (ConversionFailure "Expected Boolean")
  . preview (_JBool . _1)

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
parsur p t =
  note (ParseFailed t)
  . preview (_last . _1)
  . RP.readP_to_S p
  . BS8.unpack

pJStr :: ByteString -> Either DecodeError (JString Digit)
pJStr = parsur parseJString "JString"

-- |
-- A filthy test implementation of my filthy decoders and their respective
-- filthy parsers being wired in for the purposes of decoding from a JSON
-- bytestring into an actual Haskell type.
--
decodeTest1Json :: IO ()
decodeTest1Json = do
  cur <- mkCursor <$> BS8.readFile "test/json-data/test1.json"
  print . runIdentity . runDecoder $ mkImage cur
  where
    atKey = moveToValAtKey pJStr
    int' = int (parsur parseJNumber "JNumber")

    boolean' = boolean
      (parsur (Json <$> parseJBool parseWhitespace) "JBool")

    arr' = parsur (parseJArray parseWhitespace parseWaargonaut) "JArray"

    jnum = (jtoint <=< note (ConversionFailure "Expected JNumber")) . preview (_JNum . _1)

    mkImage curs = do
      -- We're at the root of our object, move into it
      imgObj <- moveJCurs firstChild curs
        -- At the Key of "Image"
        >>= moveJCurs nextSibling
        -- Moves to the first Key in the "Image" object
        >>= moveJCurs firstChild

      -- We need individual values off of our object,
      wVal <- int'            =<< atKey "Width" imgObj
      hVal <- int'            =<< atKey "Height" imgObj
      tVal <- text pJStr      =<< atKey "Title" imgObj
      aVal <- boolean'        =<< atKey "Animated" imgObj
      iVal <- array arr' jnum =<< atKey "IDs" imgObj

      pure $ Image wVal hVal tVal aVal iVal

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
