module Main where

import           System.Environment           (getArgs)

import           Waargonaut                   (mkParseFn, waargonautBuilder)
import           Waargonaut.Types.Json        (prettyJson)
import           Waargonaut.Types.Whitespace  (wsBuilder)
import qualified Waargonaut.Decode            as D

import qualified Data.Attoparsec.ByteString   as AB

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS

main :: IO ()
main = do
  (i: _) <- getArgs
  ibs <- BS.readFile i
  j <- D.runDecode D.json (mkParseFn AB.parseOnly) (D.mkCursor ibs)
  case j of
    Left err -> putStrLn "Oh noes!" >> putStrLn (show err)
    Right j' -> LBS.putStr $ (BS.toLazyByteString . waargonautBuilder wsBuilder . prettyJson) j'

