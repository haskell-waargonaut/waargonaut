{-# LANGUAGE RankNTypes #-}
-- | Helper functions for testing your 'Decoder' and 'Encoder' functions.
--
module Waargonaut.Test
  ( roundTripSimple
  ) where

import           Data.Text               (Text)

import qualified Data.Text.Lazy          as TextL

import           Text.Parser.Char        (CharParsing)

import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E

import           Waargonaut.Decode       (CursorHistory, Decoder)
import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (DecodeError)

-- | Test a 'Encoder' and 'Decoder' pair are able to maintain the "round trip"
-- property. That is, if you encode a given value, and then decode it, you should
-- have the exact same value that you started with.
roundTripSimple
  :: ( Eq b
     , Monad f
     , CharParsing f
     , Monad g
     , Show e
     )
  => (forall a. f a -> Text -> Either e a)
  -> Encoder g b
  -> Decoder g b
  -> b
  -> g (Either (DecodeError, CursorHistory) Bool)
roundTripSimple f e d a = do
  encodedA <- E.simpleEncodeTextNoSpaces e a
  fmap (== a) <$> D.decodeFromText f d (TextL.toStrict encodedA)
