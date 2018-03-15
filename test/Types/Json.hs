module Types.Json
  ( genJson
  , genJsons
  , genJObject
  , genJAssoc
  ) where

import           Control.Applicative         (liftA2)

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import qualified Types.JNumber               as G
import qualified Types.JString               as G
import qualified Types.LeadingTrailing       as G

import           Data.Digit                  (Digit)

import           Waargonaut.Types.Whitespace (WS)

import           Waargonaut                  (JAssoc (..), JObject (..),
                                              Json (..), Jsons (..))

-- newtype Jsons digit s = Jsons
--   { _jsonsL :: [LeadingTrailing (Json digit s) s]
genJsons :: Gen (Jsons Digit WS)
genJsons = Jsons <$> Gen.recursive Gen.choice
  [ Gen.constant []
  ]
  [ Gen.list (Range.linear 1 100) ( G.genLeadingTrailing genJson )
  ]

-- data JAssoc digit s = JAssoc
--   { _key   :: LeadingTrailing (JString digit) s
--   , _value :: LeadingTrailing (Json digit s) s
genJAssoc :: Gen (JAssoc Digit WS)
genJAssoc = Gen.recursive Gen.choice
  -- Non Recursive
  (mk . G.genLeadingTrailing <$> genJsonNonRecursive)
  -- Recursive
  [ mk $ G.genLeadingTrailing genJson
  ]
  where
    mk = liftA2 JAssoc (G.genLeadingTrailing G.genJString)

-- newtype JObject digit s = JObject
--   { _jobjectL :: [LeadingTrailing (JAssoc digit s) s]
genJObject :: Gen (JObject Digit WS)
genJObject = JObject <$> Gen.recursive Gen.choice
  [ Gen.constant []
  ]
  [ Gen.list (Range.linear 1 100) ( G.genLeadingTrailing genJAssoc )
  ]

genJsonNonRecursive :: [Gen (Json Digit WS)]
genJsonNonRecursive =
  [ JsonNull <$> G.genWS
  , JsonBool <$> Gen.bool <*> G.genWS
  , JsonNumber <$> G.genJNumber <*> G.genWS
  , JsonString <$> G.genJString <*> G.genWS
  ]

-- data Json digit s
--   = JsonNull s
--   | JsonBool Bool s
--   | JsonNumber JNumber s
--   | JsonString (JString digit) s
--   | JsonArray (Jsons digit s) s
--   | JsonObject (JObject digit s) s
genJson :: Gen (Json Digit WS)
genJson = Gen.recursive Gen.choice
  -- Non-recursive
  genJsonNonRecursive
  -- Recursive
  [ JsonArray <$> genJsons <*> G.genWS
  , JsonObject <$> genJObject <*> G.genWS
  ]
