-- | Some default decoder implementations using the @attoparsec@ package.
--
-- Waargonaut works with any parser that has an instance of the
-- 'Text.Parser.Char.CharParsing' typeclass. So you're able to select from a few
-- different parsing libraries, depending on your needs. This module provides
-- some convenient defaults using the <https://hackage.haskell.org/package/attoparsec attoparsec> package.
--
-- These functions are implemented using the @decodeFromX@ functions in the
-- @Waargonaut.Decode@ module. They use the @parseOnly@ function, from either
-- the 'Data.Attoparsec.Text' or 'Data.Attoparsec.ByteString' attoparsec modules.
--
module Waargonaut.Attoparsec
  ( -- * Decoders
    decodeAttoparsecText
  , decodeAttoparsecByteString
  , pureDecodeAttoparsecText
  , pureDecodeAttoparsecByteString
  ) where

import           Data.Functor.Identity      (Identity)

import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text       as AT

import           Waargonaut.Decode          (CursorHistory, Decoder)
import           Waargonaut.Decode.Error    (DecodeError)

import qualified Waargonaut.Decode          as D

-- | Use the 'AT.parseOnly' function as our default parser for decoding 'Text' input.
decodeAttoparsecText
  :: Monad f
  => Decoder f a
  -> Text
  -> f (Either (DecodeError, CursorHistory) a)
decodeAttoparsecText decoder =
  D.decodeFromText AT.parseOnly decoder

-- | Use the 'AB.parseOnly' function as our default parser for decoding 'ByteString' input.
decodeAttoparsecByteString
  :: Monad f
  => Decoder f a
  -> ByteString
  -> f (Either (DecodeError, CursorHistory) a)
decodeAttoparsecByteString decoder =
  D.decodeFromByteString AB.parseOnly decoder

-- | As per 'decodeAttoparsecText' but with @f@ specialised to 'Identity'.
pureDecodeAttoparsecText
  :: Decoder Identity a
  -> Text
  -> Either (DecodeError, CursorHistory) a
pureDecodeAttoparsecText decoder =
  D.pureDecodeFromText AT.parseOnly decoder

-- | As per 'decodeAttoparsecByteString' but with @f@ specialised to 'Identity'.
pureDecodeAttoparsecByteString
  :: Decoder Identity a
  -> ByteString
  -> Either (DecodeError, CursorHistory) a
pureDecodeAttoparsecByteString decoder =
  D.pureDecodeFromByteString AB.parseOnly decoder
