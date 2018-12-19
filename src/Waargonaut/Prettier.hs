{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
-- | Functions and types for pretty printing the Json data structures.
module Waargonaut.Prettier
  ( -- * Types
    InlineOption (..)

    -- * Functions
  , prettyJson
  ) where

import           Prelude                     (Eq, Int, Show, (+), (-))

import           Control.Applicative         ((<$>))
import           Control.Category            (id, (.))
import           Control.Lens                (Traversal', over, traverseOf,
                                              (%~), (.~), _1, _2, _Just,
                                              _Wrapped)

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

import           Waargonaut.Types.CommaSep   (Elems)
import qualified Waargonaut.Types.CommaSep   as CS

import           Waargonaut.Types.JObject    (HasJAssoc (..), JAssoc)
import           Waargonaut.Types.Json       (AsJType (..), JType (..), Json,
                                              jsonTraversal)
import           Waargonaut.Types.Whitespace (WS (..), Whitespace (..))

-- | Some choices for how the Json is indented.
data InlineOption
  = ArrayOnly  -- ^ Only keep array elements on the same line, input line breaks between object values.
  | ObjectOnly -- ^ Only keep object elements on the same line, input line breaks between array values.
  | Both       -- ^ Keep both object and array elements on the same line.
  | Neither    -- ^ Input line breaks for both array and object elements.
  deriving (Show, Eq)

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
  -> Int
  -> Int
  -> b
  -> b
prettyCommaSep csWrapper nested inline step w =
  setheadleadingws . stepaftercomma
  where
    spaces x = V.replicate x Space
    ws' x    = bool (WS (V.singleton NewLine) <>) id inline $ WS (spaces x)

    i = ws' (bool w 1 inline)
    l = bool (ws' (w - step)) i inline

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
prettyJson :: InlineOption -> Int -> Int -> Json -> Json
prettyJson inlineOpt step w = P.transformOf jsonTraversal (
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
      prettyJson inlineOpt step (w + step)
