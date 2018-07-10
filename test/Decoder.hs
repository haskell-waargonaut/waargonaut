{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Decoder
  ( decoderTests
  ) where

import           Test.Tasty                     (TestTree,testGroup)
import           Test.Tasty.HUnit (testCase,assertBool)

import Control.Lens (over,_Left)

import Data.Bifunctor (bimap)
import Data.Either (isLeft)

import           Data.Functor.Identity          (Identity,runIdentity)

import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS8
import           Data.Text                      (Text)

import qualified Waargonaut.Types as WT

import Waargonaut.Decode (Decoder)

import qualified Waargonaut.Decode              as D
import qualified Waargonaut.Decode.DecodeResult as D

import qualified Text.Parsec as P

import qualified Control.Zipper as Z

data Err
  = Parse P.ParseError
  | Decode (D.DecodeError, D.CursorHistory)
  deriving Show

decoderTests :: TestTree
decoderTests = testGroup "Decoding"
  [ testCase "Decode Image (test1.json)" $ decodeTest1Json >>= assertBool "Decode Image Zipper - Success" . not . isLeft
  , testCase "Decode [Int]" $ assertBool "[Int] Decode Success" (not $ isLeft decodeTest2Json)
  ]

parseBS :: ByteString -> Either P.ParseError WT.Json
parseBS = P.parse WT.parseWaargonaut "ByteString"

pureDecode
  :: Decoder Identity a
  -> D.JCursor h WT.Json
  -> Either (D.DecodeError, D.CursorHistory) a
pureDecode dec c =
  runIdentity $ D.runDecoderResult (dec c)

data Image = Image
  { _imageW        :: Int
  , _imageH        :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
  deriving Show

imageDecoder :: Monad f => Decoder f Image
imageDecoder curs = do
  -- We're at the root of our object, move into it and move to the value at the "Image" key
  o <- D.toKey "Image" curs
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int o
    <*> D.fromKey "Height" D.int o
    <*> D.fromKey "Title" D.text o
    <*> D.fromKey "Animated" D.boolean o
    <*> D.fromKey "IDs" (D.arrayOf D.int) o

simpleDecode :: (Decoder Identity a) -> ByteString -> Either Err a
simpleDecode dec bs = over _Left Decode . pureDecode dec =<< (bimap Parse Z.zipper $ parseBS bs)

decodeTest1Json :: IO (Either Err Image)
decodeTest1Json = simpleDecode imageDecoder <$> BS8.readFile "test/json-data/test1.json" 

decodeTest2Json :: Either Err [Int]
decodeTest2Json = simpleDecode (D.array D.int') "[23,44]"
