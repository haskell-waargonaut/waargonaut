{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test case from
--
-- https://gist.github.com/tonymorris/420fff469463037f5ed3f76339cb51e2
--
module Prettier.NestedObjs where

import           Data.Functor.Identity
import           Prelude

import qualified Data.Text.Lazy.Encoding as TLE

import           Data.Text.Lazy
import           Natural

import           Waargonaut.Encode
import           Waargonaut.Prettier

import           Test.Tasty              (TestTree)
import           Test.Tasty.Golden       (goldenVsString)

data Wibble =
  Wibble {
    _a1 :: String
  , _a2 :: String
  , _a3 :: String
  , _a4 :: String
  } deriving (Eq, Ord, Show)

data Wobble =
  Wobble {
    _b1 :: Wibble
  , _b2 :: Wibble
  } deriving (Eq, Ord, Show)

testWibble1 ::
  Wibble
testWibble1 =
  Wibble
    "hi1"
    "bye1"
    "hello1"
    "goodbye1"

testWibble2 ::
  Wibble
testWibble2 =
  Wibble
    "hi2"
    "bye2"
    "hello2"
    "goodbye2"

testWobble ::
  Wobble
testWobble =
  Wobble
    testWibble1
    testWibble2

wibbleEncoder ::
  Applicative f =>
  Encoder f Wibble
wibbleEncoder =
  mapLikeObj $ \w ->
    atKey' "a1" string (_a1 w) .
    atKey' "a2" string (_a2 w) .
    atKey' "a3" string (_a3 w) .
    atKey' "a4" string (_a4 w)

wobbleEncoder ::
  Applicative f =>
  Encoder f Wobble
wobbleEncoder =
  mapLikeObj $ \w ->
    atKey' "b1" wibbleEncoder (_b1 w) .
    atKey' "b2" wibbleEncoder (_b2 w)

prettyied :: Text
prettyied =
  let two = successor' (successor' zero')
  in  runIdentity (simpleEncodePretty ArrayOnly (IndentStep two) (NumSpaces two) wobbleEncoder testWobble)

-- Expected this
{-
{
  "b1": {
    "a1": "hi1",
    "a2": "bye1",
    "a3": "hello1",
    "a4": "goodbye1"
  },
  "b2": {
    "a1": "hi2",
    "a2": "bye2",
    "a3": "hello2",
    "a4": "goodbye2"
  }
}
-}
--
-- Got this
{-
{
  "b1": {
  "a1": "hi1",
  "a2": "bye1",
  "a3": "hello1",
  "a4": "goodbye1"
},
  "b2": {
    "a1": "hi2",
    "a2": "bye2",
    "a3": "hello2",
    "a4": "goodbye2"
  }
}
-}
testGoldenPrettyNested :: TestTree
testGoldenPrettyNested = goldenVsString "'encodePretty' simple nested objects"
  "test/Prettier/pretty_nested_objs.json"
  (pure $ TLE.encodeUtf8 prettyied)
