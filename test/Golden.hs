{-# LANGUAGE NoImplicitPrelude #-}
module Golden (goldenTests) where

import           Control.Applicative                  (pure, (*>))
import           Control.Category                     ((.))
import           Control.Monad                        ((>=>))

import           System.Exit                          (exitFailure)
import           System.FilePath                      (FilePath, takeBaseName)
import           System.IO                            (IO, print)

import           Data.Either                          (either)
import           Data.Function                        (($))

import           Data.ByteString.Lazy                 (ByteString, readFile,
                                                       toStrict)
import qualified Data.ByteString.Lazy.Builder         as BB

import           Data.Attoparsec.ByteString           (parseOnly)

import           Test.Tasty                           (TestTree, testGroup)
import           Test.Tasty.Golden                    (findByExtension,
                                                       goldenVsString)

import           Waargonaut.Encode.Builder            (bsBuilder)
import           Waargonaut.Encode.Builder.Whitespace (wsBuilder)

import qualified Waargonaut.Decode                    as D
import qualified Waargonaut.Encode                    as E

readAndEncodeFile :: FilePath -> IO ByteString
readAndEncodeFile = readFile
  >=> D.decodeFromByteString parseOnly D.json . toStrict
  >=> either (\err -> print err *> exitFailure) pure
  >=> E.simpleEncodeWith bsBuilder BB.toLazyByteString wsBuilder E.json

goldenTests :: IO TestTree
goldenTests = do
  fs <- findByExtension [".golden"] "test/json-data/goldens"
  pure . testGroup "Golden Tests" $
    [ goldenVsString (takeBaseName input) input (readAndEncodeFile input)
    | input <- fs
    ]
