module Types.Whitespace
  ( genWS
  , genEmptyWS
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import qualified Data.Vector as V

import           Types.Common                     (genWhitespace)

import           Waargonaut.Types.Whitespace      (WS (..))

genWS :: Gen WS
genWS = WS . V.fromList <$> Gen.list (Range.linear 0 30) genWhitespace

genEmptyWS :: Gen WS
genEmptyWS = Gen.constant mempty
