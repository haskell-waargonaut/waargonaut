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
