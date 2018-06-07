module Types.CommaSep where

import           Control.Applicative       (liftA2)

import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Data.Functor.Identity     (Identity (..))

import           Waargonaut.Types.CommaSep (Comma (..), CommaSeparated (..),
                                            Elem (..), Elems (..))

genCommaWSPair
  :: Gen ws
  -> Gen (Comma,ws)
genCommaWSPair =
  liftA2 (,) (Gen.constant Comma)

genCommaIdentity
  :: Gen ws
  -> Gen (Identity (Comma,ws))
genCommaIdentity =
  fmap Identity . genCommaWSPair

genCommaOp
  :: Gen ws
  -> Gen (Maybe (Comma,ws))
genCommaOp =
  Gen.maybe . genCommaWSPair

genEmptyCommaSeparated
  :: Gen ws
  -> Gen (CommaSeparated ws a)
genEmptyCommaSeparated gWS =
  CommaSeparated <$> gWS <*> Gen.constant Nothing

genCommaSeparated
  :: Gen ws
  -> Gen a
  -> Gen (CommaSeparated ws a)
genCommaSeparated gWS gA = Gen.recursive Gen.choice
  [ genEmptyCommaSeparated gWS
  ]
  [ CommaSeparated <$> gWS <*> Gen.maybe genCommaElems
  ]
  where
    genCommaElems = Elems
      <$> Gen.list (Range.linear 1 100) genCommaIdElem
      <*> genCommaLastElem

    genCommaIdElem = Elem <$> gA <*> genCommaIdentity gWS
    genCommaLastElem = Elem <$> gA <*> genCommaOp gWS
