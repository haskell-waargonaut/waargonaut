{-# LANGUAGE OverloadedStrings #-}
module Decoder.Parsers where

import           Control.Error.Util             (note)
import           Control.Lens                   (preview, _1, _last)

import           Data.Either                    (Either)

import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS8
import           Data.Text                      (Text)

import           Text.ParserCombinators.ReadP   (ReadP)
import qualified Text.ParserCombinators.ReadP   as RP

import           Waargonaut.Types               (JArray, JNumber, JString, Json,
                                                 WS)
import qualified Waargonaut.Types               as WT

import           Waargonaut.Decode.DecodeResult (DecodeError (..))

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
  -> Either DecodeError JString
pJStr = parsur WT.parseJString "JString"

pint
  :: ByteString
  -> Either DecodeError JNumber
pint = parsur WT.parseJNumber "JNumber"

poolean
  :: ByteString
  -> Either DecodeError Json
poolean = parsur (WT.Json <$> WT.parseJBool WT.parseWhitespace) "JBool"

parray
  :: ByteString
  -> Either DecodeError (JArray WS Json)
parray = parsur (WT.parseJArray WT.parseWhitespace WT.parseWaargonaut) "JArray"

pjnum
  :: Json
  -> Either DecodeError Int
pjnum = (note (ConversionFailure "Expected JNumber")) . preview (WT._JNum . _1 . WT._JNumberInt)

pjson
  :: ByteString
  -> Either DecodeError Json
pjson =
  parsur WT.parseWaargonaut "Json"
