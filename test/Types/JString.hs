module Types.JString
  ( genJString
  ) where

import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range

import           Data.Digit               (Digit)

import qualified Data.Vector as V

import           Types.JChar              (genJChar)

import           Waargonaut.Types.JString (JString (..))

genJString
  :: Gen (JString Digit)
genJString = JString . V.fromList <$>
  Gen.list (Range.linear 0 1000) genJChar
