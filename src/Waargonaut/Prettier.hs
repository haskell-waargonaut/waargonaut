{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
-- | Functions and types for pretty printing the Json data structures.
module Waargonaut.Prettier
  ( -- * Types
    InlineOption (..)
  , NumSpaces (..)
  , IndentStep (..)

    -- * Functions
  , prettyJson
  , simpleEncodePretty
  ) where

import           Prelude                     (Eq, Show, (+), (-))

import           Control.Applicative         (Applicative, (<$>))
import           Control.Category            (id, (.))
import           Control.Lens                (Traversal', over, traverseOf,
                                              (%~), (.~), _1, _2, _Just,
                                              _Wrapped)

import           Natural                     (Natural, minus, successor', zero',
                                              _Natural)

import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Builder      as TB

import           Data.Bool                   (Bool, bool)
import           Data.Foldable               (elem, length)
import           Data.Function               (($))
import           Data.Functor                (fmap)
import           Data.Maybe                  (maybe)
import           Data.Semigroup              ((<>))
import           Data.Traversable            (traverse)
import qualified Data.Vector                 as V

import qualified Control.Lens                as L
import qualified Control.Lens.Plated         as P

import           Waargonaut.Encode           (Encoder, runEncoder)
import           Waargonaut.Types.CommaSep   (Elems)
import qualified Waargonaut.Types.CommaSep   as CS
import           Waargonaut.Types.JObject    (HasJAssoc (..), JAssoc)
import           Waargonaut.Types.Json       (AsJType (..), JType (..), Json,
                                              jsonTraversal, waargonautBuilder)
import           Waargonaut.Types.Whitespace (WS (..), Whitespace (..),
                                              wsBuilder)

-- | Some choices for how the Json is indented.
data InlineOption
  = ArrayOnly  -- ^ Only keep array elements on the same line, input line breaks between object values.
  | ObjectOnly -- ^ Only keep object elements on the same line, input line breaks between array values.
  | Both       -- ^ Keep both object and array elements on the same line.
  | Neither    -- ^ Input line breaks for both array and object elements.
  deriving (Show, Eq)

-- | Newtype to indicate how many spaces we would like to use for the indentation
--
newtype NumSpaces = NumSpaces Natural
  deriving (Eq, Show)

-- | Newtype for how many spaces the indentation should be increased by for each level.
--
-- A safe assumption is for this value to be the same as the number of steps for
-- the identation. Such that an indentation of two spaces will be increased by
-- two for each subsequent level.
--
newtype IndentStep = IndentStep Natural
  deriving (Eq, Show)

-- | Encode an @a@ directly to a 'ByteString' using the provided 'Encoder', the
-- output will have newlines and indentation added based on the 'InlineOption' and
-- 'NumSpaces'.
simpleEncodePretty
  :: Applicative f
  => InlineOption
  -> IndentStep
  -> NumSpaces
  -> Encoder f a
  -> a
  -> f LT.Text
simpleEncodePretty io step ind enc =
  fmap (TB.toLazyText . waargonautBuilder wsBuilder . prettyJson io step ind)
  . runEncoder enc

objelems :: AsJType r WS a => Traversal' r (Elems WS (JAssoc WS a))
objelems = _JObj . _1 . _Wrapped . CS._CommaSeparated . _2 . _Just

-- I'm not sure this is a legal traversal
immediateTrailingWS :: Traversal' Json WS
immediateTrailingWS f = traverseOf _Wrapped $ \case
  JNull ws   -> JNull   <$> f ws
  JBool b ws -> JBool b <$> f ws
  JNum n ws  -> JNum n  <$> f ws
  JStr s ws  -> JStr s  <$> f ws
  JArr a ws  -> JArr a  <$> f ws
  JObj o ws  -> JObj o  <$> f ws

prettyCommaSep
  :: L.Traversal' b (CS.CommaSeparated WS a)
  -> L.Traversal' a Json
  -> Bool
  -> Natural
  -> Natural
  -> b
  -> b
prettyCommaSep csWrapper nested inline step w =
  setheadleadingws . stepaftercomma
  where
    spaces x = V.replicate (_Natural L.# x) Space
    ws' x    = bool (WS (V.singleton NewLine) <>) id inline $ WS (spaces x)

    i = ws' (bool w (successor' zero') inline)
    l = bool (ws' (w `minus` step)) i inline

    setheadleadingws   = csWrapper . CS._CommaSeparated . _1 .~ i

    stepaftercomma = csWrapper . CS._CommaSeparated . _2 . _Just %~ \es -> es
      L.& CS.elemsElems . traverse . CS.elemTrailing . fmap . _2 .~ i
      L.& CS.elemsLast . CS.elemTrailing . _Just . _2 .~ l
      L.& CS.elemsLast . CS.elemVal . nested . immediateTrailingWS .~ l

-- | Apply some indentation and spacing rules to a given Json input.
--
-- To apply newlines to object elements only and indent by two spaces,
-- increasing that indentation by two spaces for each nested object or array.
--
-- @
-- prettyJson ArrayOnly 2 2 j
-- @
--
prettyJson :: InlineOption -> IndentStep -> NumSpaces -> Json -> Json
prettyJson inlineOpt (IndentStep step) (NumSpaces w) = P.transformOf jsonTraversal (
  prettyCommaSep (_JArr . _1 . _Wrapped) id inlineArr step w .
  prettyCommaSep (_JObj . _1 . _Wrapped) jsonAssocVal inlineObj step w .
  setnested .
  alignafterkey
  )
  where
    inlineArr = inlineOpt `elem` [ArrayOnly, Both]
    inlineObj = inlineOpt `elem` [ObjectOnly, Both]

    spaces x = V.replicate x Space

    alignafterkey j = over (objelems . traverse) (\ja ->
        let
          kl = ja L.^. jsonAssocKey . _Wrapped . L.to length
        in
          ja L.& jsonAssocValPreceedingWS .~ (WS . spaces $ longestKey - kl)
      ) j
      where
        longestKey = maybe 1 (+1) $ L.maximumOf (objelems . L.folded . jsonAssocKey . _Wrapped . L.to length) j

    setnested = objelems . CS.elemsLast . CS.elemVal . jsonAssocVal %~
      prettyJson inlineOpt (IndentStep step) (NumSpaces $ w <> step)
