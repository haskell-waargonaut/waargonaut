{-# LANGUAGE RankNTypes #-}
-- |
--
-- Builder structures to help with turning 'Json' into a textual encoding.
--
module Waargonaut.Encode.Builder where

import           Data.String                       (IsString, fromString)

import           Data.Monoid                       ((<>))

import           Data.Text                         (Text)
import qualified Data.Text.Lazy.Builder            as T
import qualified Data.Text.Lazy.Builder.Int        as T

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Builder           as B

import           Waargonaut.Types.Json             (JType (..), Json (..))
import           Waargonaut.Types.Whitespace       (WS)

import           Waargonaut.Encode.Builder.JArray  (jArrayBuilder)
import           Waargonaut.Encode.Builder.JNumber (jNumberBuilder)
import           Waargonaut.Encode.Builder.JObject (jObjectBuilder)
import           Waargonaut.Encode.Builder.JString (jStringBuilder)
import           Waargonaut.Encode.Builder.Types   (Builder (..))

-- | A 'T.Text' builder
textBuilder :: Builder Text T.Builder
textBuilder = Builder
  T.singleton
  T.fromText
  T.decimal

-- | A 'B.ByteString' builder
bsBuilder :: Builder ByteString B.Builder
bsBuilder = Builder
  B.charUtf8
  B.byteString
  B.intDec

-- | A general builder function for working with 'JType' values.
--
jTypesBuilder
  :: ( IsString t
     , Monoid b
     )
  => Builder t b
  -> (Builder t b -> WS -> b)
  -> JType WS Json
  -> b
jTypesBuilder bldr s jt =
  let
    (jBuilt, tws') = case jt of
      JNull     tws -> (fromChunk bldr (fromString "null"),                          tws)
      JBool b   tws -> (fromChunk bldr (fromString $ if b then "true" else "false"), tws)
      JNum jn   tws -> (jNumberBuilder bldr jn,                                      tws)
      JStr js   tws -> (jStringBuilder bldr js,                                      tws)
      JArr js   tws -> (jArrayBuilder bldr s waargonautBuilder js,                   tws)
      JObj jobj tws -> (jObjectBuilder bldr s waargonautBuilder jobj,                tws)
  in
    jBuilt <> s bldr tws'

-- | Using the given whitespace builder, create a builder for a given 'Json' value.
waargonautBuilder
  :: ( IsString t
     , Monoid b
     )
  => (Builder t b -> WS -> b)
  -> Builder t b
  -> Json
  -> b
waargonautBuilder ws bldr (Json jt) =
  jTypesBuilder bldr ws jt
