{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           GHC.Word              (Word8)

import           Data.Either           (isLeft)

import           Data.Semigroup        ((<>))

import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Text.IO          as Text

import qualified Data.Text.Lazy        as TextL

import qualified Data.ByteString       as BS

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Waargonaut.Attoparsec as WA
import qualified Waargonaut.Decode     as D
import qualified Waargonaut.Encode     as E

import qualified Types.Common          as Common

import qualified Decoder
import qualified Decoder.Laws
import qualified Encoder
import qualified Encoder.Laws
import qualified Golden
import qualified Generics
import qualified Json
import qualified Properties

mishandlingOfCharVsUtf8Bytes :: TestTree
mishandlingOfCharVsUtf8Bytes = testCaseSteps "Mishandling of UTF-8 Bytes vs Haskell Char" $ \step -> do
  let
    valChar      = '\128'          :: Char
    valText      = "\128"          :: Text
    valStr       = [valChar]       :: String
    encVal       = "\"\128\""      :: Text
    valBytes     = [34,194,128,34] :: [Word8]

    testFilePath = "test/json-data/mishandling.json"

  step "Pack String to Text"
  Text.pack valStr @?= valText

  step "Encoder via Text"
  let x = TextL.toStrict $ Common.encodeText E.text valText
  x @?= encVal

  step "Create JSON file"
  Text.writeFile testFilePath x

  step "encoder output ~ packed bytes"
  Text.encodeUtf8 x @?= BS.pack valBytes

  step "Decode file input"
  decodedFile <- WA.decodeAttoparsecText D.text =<< Text.readFile testFilePath
  decodedFile @?= Right valText

regressionTests :: TestTree
regressionTests = testGroup "Expected Failure" $
  toTestFail <$> fs
  where
    toTestFail (dsc, f) = testCase dsc $ do
      r <- WA.decodeAttoparsecText D.json =<< Text.readFile ("test/json-data/bad-json/" <> f)
      assertBool (f <> " should fail to parse!") (isLeft r)

    fs =
      [ ("[11 12 13] (test4.json)","no_comma_arr.json")
      , ("{\"foo\":3\"bar\":4} (test6.json)", "no_comma_obj.json")
      ]

main :: IO ()
main = do
  goldens <- Golden.goldenTests
  defaultMain $ testGroup "Waargonaut All Tests"
    [ regressionTests
    , mishandlingOfCharVsUtf8Bytes

    , Properties.propertyTests
    , Json.jsonPrisms
    , Decoder.Laws.decoderLaws
    , Encoder.Laws.encoderLaws

    , Decoder.decoderTests
    , Encoder.encoderTests

    , Generics.genericsTests

    , goldens
    ]
