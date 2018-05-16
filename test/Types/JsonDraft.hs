{-# LANGUAGE TupleSections #-}
module Types.JsonDraft
  ( genJson
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import qualified Types.JNumber               as G
import qualified Types.JString               as G
import qualified Types.LeadingTrailing       as G

import           Data.Digit                  (Digit)

import           Data.Functor.Identity       (Identity (..))

import           Waargonaut.Types.CommaSep   (Comma (..))
import           Waargonaut.Types.Whitespace (WS)

import           WaargDraft                  (JArray (..), JObject (..),
                                              JTypes (..), Json (..),
                                              JsonArr (..), JsonArrElem (..),
                                              JsonAssoc (..), JsonObj (..),
                                              JsonObjElem (..))

genCommaIdentity :: Gen (Identity (Comma,WS))
genCommaIdentity = fmap Identity $ (,) <$> Gen.constant Comma <*> G.genWS

genCommaOp :: Gen (Maybe (Comma,WS))
genCommaOp = Gen.maybe $ (,) <$> Gen.constant Comma <*> G.genWS

genJsonArrLastElem :: Gen (JsonArrElem Maybe WS Json)
genJsonArrLastElem = JsonArrElem <$> genJson <*> genCommaOp

genJsonArrElem' :: Gen (JsonArrElem Identity WS Json)
genJsonArrElem' = JsonArrElem <$> genJson <*> genCommaIdentity

genJsonArr :: Gen (JsonArr WS Json)
genJsonArr = Gen.recursive Gen.choice
    [ JsonArr <$> Gen.constant [] <*> genJsonArrLastElem
    ]
    [ JsonArr <$> genJElems <*> genJsonArrLastElem
    ]
  where
    genJElems = Gen.list (Range.linear 0 100) genJsonArrElem'

genEmptyJArray :: Gen (JArray WS Json)
genEmptyJArray = JArray <$> G.genWS <*> Gen.constant Nothing

genJArray :: Gen (JArray WS Json)
genJArray = Gen.recursive Gen.choice
  [ JArray <$> G.genWS <*> Gen.constant Nothing ]
  [ JArray <$> G.genWS <*> Gen.maybe genJsonArr ]

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

genJsonObjLastElem :: Gen (JsonObjElem Maybe Digit WS Json)
genJsonObjLastElem = JsonObjElem <$> genJAssoc <*> genCommaOp

genJsonObjElem :: Gen (JsonObjElem Identity Digit WS Json)
genJsonObjElem = JsonObjElem <$> genJAssoc <*> genCommaIdentity

genJsonObj :: Gen (JsonObj Digit WS Json)
genJsonObj = Gen.recursive Gen.choice
  [ JsonObj <$> Gen.constant [] <*> genJsonObjLastElem ]
  [ JsonObj <$> Gen.list (Range.linear 1 100) genJsonObjElem <*> genJsonObjLastElem ]

genJObj :: Gen (JObject Digit WS Json)
genJObj = Gen.recursive Gen.choice
  [ JObject <$> G.genWS <*> Gen.constant Nothing ]
  [ JObject <$> G.genWS <*> Gen.maybe genJsonObj ]

genJsonNonRecursive :: [Gen Json]
genJsonNonRecursive = fmap (fmap Json)
  [ Gen.constant JNull <*> G.genWS
  , JBool <$> Gen.bool  <*> G.genWS
  , JNum  <$> G.genJNumber <*> G.genWS
  , JStr  <$> G.genJString <*> G.genWS
  , JArr  <$> genEmptyJArray <*> G.genWS
  ]

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
