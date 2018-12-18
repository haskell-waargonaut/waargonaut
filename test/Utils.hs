{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Control.Applicative     ((<$>), (<*), (<*>))

import           Data.Char               (Char)
import           Data.Either             (Either (..))
import           Data.Text               (Text)

import           Data.Attoparsec.Text    (Parser, anyChar, endOfInput,
                                          parseOnly)

import           Waargonaut.Decode       (parseWith)
import           Waargonaut.Decode.Error (DecodeError)

testparse
  :: Parser a
  -> Text
  -> Either DecodeError a
testparse p =
  parseWith parseOnly p

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
