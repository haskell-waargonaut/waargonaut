{-# LANGUAGE TupleSections #-}
module Types.Json
  ( genJson
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                as Gen

import qualified Types.JNumber               as G
import qualified Types.JString               as G
import qualified Types.Whitespace            as G

import           Data.Digit                  (Digit)

import           Waargonaut.Types.Whitespace (WS)

import           Waargonaut                  (JArray (..), JObject (..),
                                              JTypes (..), Json (..),
                                              JsonAssoc (..))

import           Types.CommaSep              (genCommaSeparated,
                                              genEmptyCommaSeparated)

genJArray :: Gen (JArray WS Json)
genJArray = JArray <$> genCommaSeparated G.genWS genJson

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
      <*> G.genWS
      <*> v

genJObj :: Gen (JObject Digit WS Json)
genJObj = JObject <$> genCommaSeparated G.genWS genJAssoc

genJsonNonRecursive :: [Gen Json]
genJsonNonRecursive = (fmap . fmap) Json
  [ JNull <$> G.genWS
  , JBool <$> Gen.bool     <*> G.genWS
  , JNum  <$> G.genJNumber <*> G.genWS
  , JStr  <$> G.genJString <*> G.genWS
  , emptyCommaSep JArr JArray
  , emptyCommaSep JObj JObject
  ]
  where
    emptyCommaSep oc c =
      oc <$> (c <$> genEmptyCommaSeparated G.genWS) <*> G.genWS

genJson :: Gen Json
genJson = Gen.recursive Gen.choice
  -- Non-recursive
  genJsonNonRecursive
  -- Recursive
  [ mk $ JArr <$> genJArray <*> G.genWS
  , mk $ JObj <$> genJObj <*> G.genWS
  ]
  where
    mk = fmap Json
