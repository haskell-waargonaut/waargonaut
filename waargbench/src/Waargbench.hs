{-# LANGUAGE RankNTypes #-}
module Main (main) where

import           Control.Applicative                          (liftA2)

import           Data.Functor.Identity                        (Identity)

import           Data.ByteString                              (ByteString)
import qualified Data.ByteString                              as BS
import           Data.Either                                  (isRight)
import           Data.Maybe                                   (isJust)
import           Data.Monoid                                  ((<>))

import           Data.Attoparsec.ByteString                   (parseOnly)

import qualified Criterion.Main                               as G
import qualified HaskellWorks.Data.Json.Internal.Cursor.Token as HW

import qualified Waargonaut                                   as W

import           Waargonaut.Decode                            (Decoder)
import qualified Waargonaut.Decode                            as D

import           Common                                       (decodeScientific,
                                                               imageDecode,
                                                               imageDecodeGeneric)

parseOkay :: ByteString -> Bool
parseOkay = isRight . parseOnly W.parseWaargonaut

indexOkay :: ByteString -> Bool
indexOkay = isJust . HW.jsonTokenAt . D.unJCurs . D.mkCursor

decodeByteString :: Decoder Identity a -> ByteString -> Bool
decodeByteString d = isRight . D.pureDecodeFromByteString parseOnly d

rf :: FilePath -> IO ByteString
rf f = BS.readFile $ "../test/json-data/" <> f

getParseFiles :: IO [ByteString]
getParseFiles = sequence
  [ (rf "jp100.json")
  , (rf "twitter100.json")
  , (rf "numbers.json")
  ]

getDecodeFiles :: IO (ByteString, ByteString)
getDecodeFiles = liftA2 (,)
  (rf "test1.json")
  (rf "numbers.json")

main :: IO ()
main = G.defaultMain
  [ parse
  , parseSuccinct
  , decode
  ]

decode :: G.Benchmark
decode = G.env getDecodeFiles $ \ ~(image, numbers) -> G.bgroup "Decode"
  [ G.bench "Image Decode (generic)" $ G.nf (decodeByteString imageDecodeGeneric) image
  , G.bench "Image Decode" $ G.nf (decodeByteString imageDecode) image
  , G.bench "[Scientific]" $ G.nf (decodeByteString decodeScientific) numbers
  ]

parse :: G.Benchmark
parse = G.env getParseFiles $ \ ~(jp100 :twitter100:numbers:_) -> G.bgroup "Parse - Attoparsec"
  [ G.bench "jp100"                    $ G.nf parseOkay jp100
  , G.bench "twitter100"               $ G.nf parseOkay twitter100
  , G.bench "numbers"                  $ G.nf parseOkay numbers
  ]

parseSuccinct :: G.Benchmark
parseSuccinct = G.env getParseFiles $ \ ~(jp100:twitter100:numbers:_) -> G.bgroup "Succinct Index"
  [ G.bench "jp100"                    $ G.nf indexOkay jp100
  , G.bench "twitter100"               $ G.nf indexOkay twitter100
  , G.bench "numbers"                  $ G.nf indexOkay numbers
  ]
