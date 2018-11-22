{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Control.Applicative     ((<$>), (<*), (<*>))
import           Control.Category        ((.))

import           Data.Attoparsec.Text    (Parser, anyChar, endOfInput,
                                          parseOnly)
import           Data.Bifunctor          (first)
import           Data.Char               (Char)
import           Data.Either             (Either (..))
import           Data.Text               (Text, pack)

import           Waargonaut.Decode.Error (DecodeError (ParseFailed))

testparse
  :: Parser a
  -> Text
  -> Either DecodeError a
testparse p =
  first (ParseFailed . pack) . parseOnly p

testparsetheneof
  :: Parser a
  -> Text
  -> Either DecodeError a
testparsetheneof p =
  testparse (p <* endOfInput)

testparsethennoteof
  :: Parser a
  -> Text
  -> Either DecodeError a
testparsethennoteof p =
  testparse (p <* anyChar)

testparsethen
  :: Parser a
  -> Text
  -> Either DecodeError (a, Char)
testparsethen p =
  testparse ((,) <$> p <*> anyChar <* endOfInput)
