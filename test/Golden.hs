{-# LANGUAGE NoImplicitPrelude #-}
module Golden (goldenTests) where

import           Control.Applicative        (pure, (*>))
import           Control.Category           ((.))
import           Control.Monad              ((>=>))

import           System.Exit                (exitFailure)
import           System.FilePath            (FilePath, takeBaseName)
import           System.IO                  (IO, print)

import           Data.Either                (either)
import           Data.Function              (($))

import           Data.ByteString.Lazy       (ByteString, readFile, toStrict)

import           Data.Attoparsec.ByteString (parseOnly)

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Encode          as E

readAndEncodeFile :: FilePath -> IO ByteString
readAndEncodeFile = readFile
  >=> D.decodeFromByteString parseOnly D.json . toStrict
  >=> either (\err -> print err *> exitFailure) pure
  >=> E.simpleEncodeByteString E.json

goldenTests :: IO TestTree
goldenTests = do
  fs <- findByExtension [".golden"] "test/json-data/goldens"
  pure . testGroup "Golden Tests" $
    [ goldenVsString (takeBaseName input) input (readAndEncodeFile input)
    | input <- fs
    ]
