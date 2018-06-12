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

import           Waargonaut.Types.JArray     (JArray (..))
import           Waargonaut.Types.JObject    (JObject (..), JAssoc (..), JAssocKey (..))

import           Waargonaut                  (JTypes (..), Json (..))

import           Types.CommaSep              (genCommaSeparated,
                                              genEmptyCommaSeparated)

genJArray :: Gen (JArray WS Json)
genJArray = JArray <$> genCommaSeparated G.genWS genJson

genJAssoc :: Gen (JAssoc Digit WS Json)
genJAssoc = Gen.recursive Gen.choice
  -- Non Recursive
  (mk <$> genJsonNonRecursive)
  -- Recursive
  [ mk genJson ]
  where
    mk v = JAssoc
      <$> fmap JAssocKey G.genJString
      <*> G.genWS
      <*> G.genWS
      <*> v

genJObj :: Gen (JObject Digit WS Json)
genJObj = JObject <$> genCommaSeparated G.genWS genJAssoc

toJson
  :: (t -> WS -> JTypes Digit WS Json)
  -> Gen t
  -> Gen Json
toJson c v =
  (\v' -> Json . c v') <$> v <*> G.genWS

genJsonNonRecursive :: [Gen Json]
genJsonNonRecursive =
  [ toJson (const JNull) G.genWS
  , toJson JBool Gen.bool
  , toJson JNum G.genJNumber
  , toJson JStr G.genJString
  , emptyCommaSep JArr JArray
  , emptyCommaSep JObj JObject
  ]
  where
    emptyCommaSep oc c = Json <$> (
      oc . c <$> genEmptyCommaSeparated G.genWS <*> G.genWS
      )

genJson :: Gen Json
genJson = Gen.recursive Gen.choice
  -- Non-recursive
  genJsonNonRecursive
  -- Recursive
  [ toJson JArr genJArray
  , toJson JObj genJObj
  ]
