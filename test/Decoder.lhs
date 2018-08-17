Decoding with Waargonaut.
=========================

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Decoder
  ( decoderTests
  ) where
\end{code}

\begin{code}
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, testCase)

import           Data.Either                (isLeft)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS8

import           Waargonaut.Decode          (Err, Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Types           as WT

import qualified Text.Parsec                as P
\end{code}

\begin{code}
import           Types.Common               (Image (Image))
\end{code}

\begin{code}
decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)"
    $ decodeTest1Json >>= assertBool "Decode Image Zipper - Success" . not . isLeft

  , testCase "Decode [Int]"
    $ assertBool "[Int] Decode Success" (not $ isLeft decodeTest2Json)
  ]
\end{code}

\begin{code}
parseBS :: ByteString -> Either P.ParseError WT.Json
parseBS = P.parse WT.parseWaargonaut "ByteString"
\end{code}

\begin{code}
decodeTest2Json :: Either (Err P.ParseError) [Int]
decodeTest2Json = D.simpleDecode parseBS (D.arrayOf D.int) "[23,44]"
\end{code}

\begin{code}
imageDecoder :: Monad f => Decoder f Image
imageDecoder = D.withCursor $ \curs -> do
  -- We're at the root of our object, move into it and move to the value at the "Image" key
  o <- D.moveToKey "Image" curs
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int o
    <*> D.fromKey "Height" D.int o
    <*> D.fromKey "Title" D.text o
    <*> D.fromKey "Animated" D.boolean o
    <*> D.fromKey "IDs" (D.arrayOf D.int) o
\end{code}

\begin{code}
decodeTest1Json :: IO (Either (Err P.ParseError) Image)
decodeTest1Json = D.simpleDecode parseBS imageDecoder <$> BS8.readFile "test/json-data/test1.json"
\end{code}
