{-# LANGUAGE LambdaCase #-}
module Waargonaut.Decode.Error
  ( DecodeError (..)
  , AsDecodeError (..)
  ) where

import           Control.Lens                 (Prism')
import qualified Control.Lens                 as L

import           GHC.Word                     (Word64)

import           Data.Text                    (Text)

import           Waargonaut.Decode.ZipperMove (ZipperMove)

import           Waargonaut.Types             (JNumber)

-- |
-- Set of errors that may occur during the decode phase.
--
data DecodeError
  = ConversionFailure Text
  | KeyDecodeFailed Text
  | KeyNotFound Text
  | FailedToMove ZipperMove
  | NumberOutOfBounds JNumber
  | InputOutOfBounds Word64
  | ParseFailed Text
  deriving (Show, Eq)

class AsDecodeError r where
  _DecodeError       :: Prism' r DecodeError
  _ConversionFailure :: Prism' r Text
  _KeyDecodeFailed   :: Prism' r Text
  _KeyNotFound       :: Prism' r Text
  _FailedToMove      :: Prism' r ZipperMove
  _NumberOutOfBounds :: Prism' r JNumber
  _InputOutOfBounds  :: Prism' r Word64
  _ParseFailed       :: Prism' r Text

  _ConversionFailure = _DecodeError . _ConversionFailure
  _KeyDecodeFailed   = _DecodeError . _KeyDecodeFailed
  _KeyNotFound       = _DecodeError . _KeyNotFound
  _FailedToMove      = _DecodeError . _FailedToMove
  _NumberOutOfBounds = _DecodeError . _NumberOutOfBounds
  _InputOutOfBounds  = _DecodeError . _InputOutOfBounds
  _ParseFailed       = _DecodeError . _ParseFailed

instance AsDecodeError DecodeError where
  _DecodeError = id

  _ConversionFailure
    = L.prism ConversionFailure
        (\x -> case x of
            ConversionFailure y -> Right y
            _                   -> Left x
        )

  _KeyDecodeFailed
    = L.prism KeyDecodeFailed
        (\x -> case x of
            KeyDecodeFailed y -> Right y
            _                 -> Left x
        )

  _KeyNotFound
    = L.prism KeyNotFound
        (\x -> case x of
            KeyNotFound y -> Right y
            _             -> Left x
        )

  _FailedToMove
    = L.prism FailedToMove
        (\x -> case x of
            FailedToMove y -> Right y
            _              -> Left x
        )

  _NumberOutOfBounds
    = L.prism NumberOutOfBounds
        (\x -> case x of
            NumberOutOfBounds y -> Right y
            _                   -> Left x
        )

  _InputOutOfBounds
    = L.prism InputOutOfBounds
      (\x -> case x of
          InputOutOfBounds y -> Right y
          _                  -> Left x
      )

  _ParseFailed
    = L.prism ParseFailed
        (\x -> case x of
            ParseFailed y -> Right y
            _             -> Left x
        )
