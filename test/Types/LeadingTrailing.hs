module Types.LeadingTrailing
  ( genLeadingTrailing
  , genWS
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import           Types.Common                     (genWhitespace)

import           Waargonaut.Types.LeadingTrailing (LeadingTrailing (..))
import           Waargonaut.Types.Whitespace      (WS (..))

genWS :: Gen WS
genWS = WS <$> Gen.list (Range.linear 0 30) genWhitespace

genLeadingTrailing :: Gen a -> Gen (LeadingTrailing a WS)
genLeadingTrailing genInner = LeadingTrailing <$> genWS <*> genInner <*> genWS
