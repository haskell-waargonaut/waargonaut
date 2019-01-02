-- | Helper functions for testing your 'Decoder' and 'Encoder' functions.
--
module Waargonaut.Test
  ( roundTripSimple
  ) where

import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as TextL

import           Data.ByteString         (ByteString)

import           Text.Parser.Char        (CharParsing)

import           Waargonaut.Types        (Json, parseWaargonaut)

import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E

import           Waargonaut.Decode       (CursorHistory, Decoder)
import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (DecodeError)

-- | Test a 'Encoder' and 'Decoder' pair are able to maintain the 'round trip'
-- property. That is, if you encode a given value, and then decode it, you should
-- have the exact same value that you started with.
roundTripSimple
  :: ( Eq b
     , Monad f
     , CharParsing f
     , Monad g
     , Show e
     )
  => (f Json -> ByteString -> Either e Json)
  -> Encoder g b
  -> Decoder g b
  -> b
  -> g (Either (DecodeError, CursorHistory) Bool)
roundTripSimple f e d a = do
  encodedA <- E.simpleEncodeNoSpaces e a
  (fmap . fmap) (== a) $ D.runDecode d
    (D.parseWith f parseWaargonaut)
    (D.mkCursor . Text.encodeUtf8 . TextL.toStrict $ encodedA)
