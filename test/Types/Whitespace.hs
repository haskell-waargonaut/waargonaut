module Types.Whitespace
  ( genWS
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import           Types.Common                     (genWhitespace)

import           Waargonaut.Types.Whitespace      (WS (..))

genWS :: Gen WS
genWS = WS <$> Gen.list (Range.linear 0 30) genWhitespace
