{-# LANGUAGE RankNTypes #-}
module Waargonaut.Decode.Runners
  (
    -- * General over @f@
    decodeWithInput
  , decodeFromStringLossy
  , decodeFromText
  , decodeFromByteString

    -- * Identity
  , pureDecodeWithInput
  , pureDecodeFromText
  , pureDecodeFromByteString
  , pureDecodeFromStringLossy

    -- * Helpers
  , overrideParser
  , parseWith

  ) where

import           Prelude                    (Show, String, show)

import           Control.Category           (id, (.))
import           Control.Monad              (Monad (..))

import           Control.Monad.Reader       (local)

import           Data.Bifunctor             (first)
import           Data.Either                (Either (..))
import           Data.Function              (const)
import           Data.Functor.Identity      (Identity, runIdentity)

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text

import           Text.Parser.Char           (CharParsing)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS8

import           Waargonaut.Decode.Error    (DecodeError (..))
import           Waargonaut.Types

import qualified Waargonaut.Decode.Internal as DI

import           Waargonaut.Decode.Types    (CursorHistory, DecodeResult (..),
                                             Decoder (..), mkCursor)

-- | Run a 'Decoder' for the final result to see if you have your 'a' or an error.
-- runDecode
--   :: Monad f
--   => Decoder f a
--   -> ParseFn
--   -> JCurs
--   -> f (Either (DecodeError, CursorHistory) a)
-- runDecode dr p =
--   DI.runDecoderResultT . runDecoder dr p

-- |
-- Using the 'ParseFn', complete a 'DecodeResult' to find out if we have the type we're after. This
-- is mostly used internally to help build 'Decoder' structures. Exported as it may prove useful
-- when abstracting over the 'Decoder' types or other such shenanigans.
-- runDecodeResult
--   :: Monad f
--   => ParseFn
--   -> DecodeResult f a
--   -> f (Either (DecodeError, CursorHistory) a)
-- runDecodeResult p =
--   DI.runDecoderResultT
--   . flip runReaderT p
--   . unDecodeResult

-- | General decoding function that takes a given parsing function and some
-- functions to handle the transition from the input of the 'JCurs' to the
-- desired input type. The indexer and cursor requires a 'ByteString' to work
-- efficiently, but this does not preclude the use of other text types, provided
-- the right functions are present.
--
-- There are some specialised versions of this function provided for 'Text',
-- 'String', and 'ByteString'. They are implemented using this function, for
-- example to work with 'Text' input and the 'attoparsec' package:
--
-- @
-- import qualified Data.Attoparsec.Text as AT
-- import qualified Data.Text as Text
--
-- textDecode :: Monad g => Decoder g x -> Text -> g (Either (DecodeError, CursorHistory) x)
-- textDecode = decodeWithInput AT.parseOnly Text.decodeUtf8 Text.encodeUtf8
-- @
--
decodeWithInput
  :: ( CharParsing f
     , Show e
     , Monad g
     , Monad f
     )
  => (forall a. f a -> i -> Either e a)
  -> (ByteString -> i)
  -> (i -> ByteString)
  -> Decoder g x
  -> i
  -> g (Either (DecodeError, CursorHistory) x)
decodeWithInput parserFn toI fromI decode = DI.runDecoderResultT
  . runDecoder decode (parseWith parserFn parseWaargonaut . toI)
  . mkCursor
  . fromI

-- | As per the 'decodeWithInput' function, but with the input type specialised
-- to 'String'. Please note that this function is lossy with respect to any
-- 'Char' that is _not_ in the 0-255 range:
--
-- * Unicode Basic Latin
-- * Latin-1 Supplement
-- * C0+C1 Controls.
--
-- This is due to the use of 'Data.ByteString.Char8' functions to manage the
-- conversion.
--
decodeFromStringLossy
  :: ( CharParsing f
     , Monad f
     , Monad g
     , Show e
     )
  => (forall a. f a -> String -> Either e a)
  -> Decoder g x
  -> String
  -> g (Either (DecodeError, CursorHistory) x)
decodeFromStringLossy parseFn =
  decodeWithInput parseFn BS8.unpack BS8.pack

-- | As per 'decodeWithInput' function but specialised to the 'ByteString' input type.
decodeFromByteString
  :: ( CharParsing f
     , Monad f
     , Monad g
     , Show e
     )
  => (forall a. f a -> ByteString -> Either e a)
  -> Decoder g x
  -> ByteString
  -> g (Either (DecodeError, CursorHistory) x)
decodeFromByteString parseFn =
  decodeWithInput parseFn id id

-- | As per 'decodeWithInput' function but specialised to the 'Text' input type.
--
-- An example:
--
-- @
-- textDecode :: Decoder f a -> f (Either (DecodeError, CursorHistory) a)
-- textDecode = decodeFromText AT.parseOnly
-- @
--
decodeFromText
  :: ( CharParsing f
     , Monad f
     , Monad g
     , Show e
     )
  => (forall a. f a -> Text -> Either e a)
  -> Decoder g x
  -> Text
  -> g (Either (DecodeError, CursorHistory) x)
decodeFromText parseFn =
  decodeWithInput parseFn Text.decodeUtf8 Text.encodeUtf8

-- | This function works using one of the 'decodeFrom*' functions and provides a
-- pure decoder that demands the 'Decoder' but specialised to 'Identity'.
-- 'Decoder's are often to be general in their @g@ type so most are fine to use
-- this function. It is offered as a convenience and can be used like so:
--
-- @
-- import qualified Data.Attoparsec.Text as AT
-- import qualified Data.Text as Text
--
-- pureTextDecode :: Decoder Identity x -> Text -> Either (DecodeError, CursorHistory) x
-- pureTextDecode = pureDecodeWithInput decodeWithText AT.parseOnly
-- @
--
pureDecodeWithInput
  :: ( Monad f
     , CharParsing f
     , Show e
     )
  => ( forall g. Monad g
       => (forall a. f a -> i -> Either e a)
       -> Decoder g x
       -> i
       -> g (Either (DecodeError, CursorHistory) x)
     )
  -> (forall a. f a -> i -> Either e a)
  -> Decoder Identity x
  -> i
  -> Either (DecodeError, CursorHistory) x
pureDecodeWithInput decodeRunner parseFn decoder =
  runIdentity . decodeRunner parseFn decoder

-- | As per 'pureDecodeWithInput' but specialised to 'Data.Text.Text'.
pureDecodeFromText
  :: ( Monad f
     , CharParsing f
     , Show e
     )
  => (forall a. f a -> Text -> Either e a)
  -> Decoder Identity x
  -> Text
  -> Either (DecodeError, CursorHistory) x
pureDecodeFromText =
  pureDecodeWithInput decodeFromText

-- | As per 'pureDecodeWithInput' but specialised to 'Data.ByteString.ByteString'.
pureDecodeFromByteString
  :: ( Monad f
     , CharParsing f
     , Show e
     )
  => (forall a. f a -> ByteString -> Either e a)
  -> Decoder Identity x
  -> ByteString
  -> Either (DecodeError, CursorHistory) x
pureDecodeFromByteString =
  pureDecodeWithInput decodeFromByteString

-- | As per 'pureDecodeWithInput' but specialised to 'Data.String.String'.
--
-- This function is affected in the same way as 'decodeFromString' with respect
-- to only working with characters that appear in the 0-255 range.
--
pureDecodeFromStringLossy
  :: ( Monad f
     , CharParsing f
     , Show e
     )
  => (forall a. f a -> String -> Either e a)
  -> Decoder Identity x
  -> String
  -> Either (DecodeError, CursorHistory) x
pureDecodeFromStringLossy =
  pureDecodeWithInput decodeFromStringLossy

-- | Helper function to handle wrapping up a parse failure using the given
-- parsing function. Intended to be used with the 'runDecode' or 'simpleDecode'
-- functions.
--
-- @
-- import Data.Attoparsec.ByteString (parseOnly)
--
-- simpleDecode (list int) (parseWith (parseOnly parseWaargonaut)) "[1,1,2]"
-- @
--
parseWith
  :: ( CharParsing f
     , Show e
     )
  => (f a -> i -> Either e a)
  -> f a
  -> i
  -> Either DecodeError a
parseWith f p =
  first (ParseFailed . Text.pack . show) . f p

-- | This function lets you override the parsing function that is being used in
-- a decoder for a different one. This means that when building your 'Decoder' you
-- are not bound to only using a single parsing function. If you have specific
-- needs for alternate parsers then you can use this function in your 'Decoder' to
-- make that change.
--
-- Similar to the other decoding functions, this operation allows you to specify
-- your own parsing function and if necessary a 'ByteString -> i' conversion.
--
-- @
--
-- parseUsingByteString :: (Show e, CharParsing f, Monad f) => f a -> ByteString -> Either e a
-- parseUsingByteString = ...
--
-- myTricksyObj = withCursor $ \curs -> do
--   curs' <- down curs
--   fA <- fromKey "normalFieldA" int curs'
--   fB <- fromKey "normalFieldB" text curs'
--   wB <- overrideParser parseUsingByteString id handTunedParser $ fromKey "weirdFieldC" fieldCDecoder curs'
--   pure $ Foo fA fB wB
-- @
--
overrideParser
  :: ( CharParsing g
     , Monad g
     , Monad f
     , Show e
     )
  => (forall x. g x -> i -> Either e x)
  -> (ByteString -> i)
  -> g Json
  -> DecodeResult f a
  -> DecodeResult f a
overrideParser newParseFn floop parser =
  local (const (parseWith newParseFn parser . floop))
