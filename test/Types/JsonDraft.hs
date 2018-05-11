{-# LANGUAGE TupleSections #-}
module Types.JsonDraft
  ( genJson
  ) where

import           Control.Applicative         (liftA2)

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import qualified Types.JNumber               as G
import qualified Types.JString               as G
import qualified Types.LeadingTrailing       as G

import           Data.Digit                  (Digit)
import           Data.Separated              (Separated (..))

import           Waargonaut.Types.Whitespace (WS)

import           WaargDraft                  (Comma (..), JTypes (..),
                                              Json (..), JsonArr (..),
                                              JsonAssoc (..), JsonObj (..))


genJsonArr :: Gen (JsonArr WS Json)
genJsonArr = JsonArr
  <$> G.genWS
  <*> Gen.recursive Gen.choice [ Gen.constant [] ] [ Gen.list (Range.linear 1 100) genJson ]

genCommaOp :: Gen (Maybe Comma)
genCommaOp = Gen.maybe (Gen.constant Comma)

genJAssoc :: Gen (JsonAssoc Digit WS Json)
genJAssoc = Gen.recursive Gen.choice
  -- Non Recursive
  (mk <$> genJsonNonRecursive)
  -- Recursive
  [ mk genJson ]
  where
    mk v = JsonAssoc
      <$> G.genJString
      <*> G.genWS
      <*> v
      <*> genCommaOp
      <*> G.genWS

genJObj :: Gen (JsonObj Digit WS Json)
genJObj = JsonObj <$> G.genWS <*> Gen.recursive Gen.choice
  [ Gen.constant [] ]
  [ Gen.list (Range.linear 1 100) genJAssoc ]

genJsonNonRecursive :: [Gen Json]
genJsonNonRecursive = fmap (liftA2 Json G.genWS)
  [ Gen.constant JNull <*> G.genWS
  , JBool <$> Gen.bool  <*> G.genWS
  , JNum <$> G.genJNumber <*> G.genWS
  , JStr <$> G.genJString <*> G.genWS
  ]

genJson :: Gen Json
genJson = Gen.recursive Gen.choice
  -- Non-recursive
  genJsonNonRecursive
  -- Recursive
  [ mk $ JArr <$> genJsonArr <*> G.genWS
  , mk $ JObj <$> genJObj <*> G.genWS
  ]
  where
    mk = liftA2 Json G.genWS
