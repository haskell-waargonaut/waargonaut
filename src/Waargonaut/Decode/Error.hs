{-# LANGUAGE DeriveFunctor #-}
-- | Types and typeclass for errors in Waargonaut decoding.
module Waargonaut.Decode.Error
  ( DecodeError (..)
  , AsDecodeError (..)
  , Err (..)
  ) where

import           Control.Lens                 (Prism')
import qualified Control.Lens                 as L

import           GHC.Word                     (Word64)

import           HaskellWorks.Data.Json.Standard.Cursor.Type  (JsonType)

import           Data.Text                    (Text)

import           Waargonaut.Decode.ZipperMove (ZipperMove)

import           Waargonaut.Types             (JNumber)

-- | Convenience Error structure for the separate parsing/decoding phases. For
-- when things really aren't that complicated.
data Err c e
  = Parse e
  | Decode (DecodeError, c)
  deriving (Show, Eq, Functor)

-- |
-- Set of errors that may occur during the decode phase.
--
data DecodeError
  = ConversionFailure Text
  | TypeMismatch JsonType
  | KeyDecodeFailed
  | KeyNotFound Text
  | FailedToMove ZipperMove
  | NumberOutOfBounds JNumber
  | InputOutOfBounds Word64
  | ParseFailed Text
  deriving (Show, Eq)

-- | Describes the sorts of errors that may be treated as a 'DecodeError', for use with 'Control.Lens.Prism's.
class AsDecodeError r where
  _DecodeError       :: Prism' r DecodeError
  _ConversionFailure :: Prism' r Text
  _TypeMismatch      :: Prism' r JsonType
  _KeyDecodeFailed   :: Prism' r ()
  _KeyNotFound       :: Prism' r Text
  _FailedToMove      :: Prism' r ZipperMove
  _NumberOutOfBounds :: Prism' r JNumber
  _InputOutOfBounds  :: Prism' r Word64
  _ParseFailed       :: Prism' r Text

  _ConversionFailure = _DecodeError . _ConversionFailure
  _TypeMismatch      = _DecodeError . _TypeMismatch
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

  _TypeMismatch
    = L.prism TypeMismatch
        (\x -> case x of
            TypeMismatch y -> Right y
            _              -> Left x
        )

  _KeyDecodeFailed
    = L.prism (const KeyDecodeFailed)
        (\x -> case x of
            KeyDecodeFailed -> Right ()
            _               -> Left x
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
