{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils where

import           Control.Applicative ((<$>), (<*), (<*>))

import           Data.Char           (Char)
import           Data.Either         (Either (..))

import           Data.Text           (Text)

import           Text.Parsec         (ParseError, Parsec, anyChar, eof, parse)

testparse
  :: Parsec Text () a
  -> Text
  -> Either ParseError a
testparse p =
  parse p "test"

testparsetheneof
  :: Parsec Text () a
  -> Text
  -> Either ParseError a
testparsetheneof p =
  testparse (p <* eof)

testparsethennoteof
  :: Parsec Text () a
  -> Text
  -> Either ParseError a
testparsethennoteof p =
  testparse (p <* anyChar)

testparsethen
  :: Parsec Text () a
  -> Text
  -> Either ParseError (a, Char)
testparsethen p =
  parse ((,) <$> p <*> anyChar) "test"
