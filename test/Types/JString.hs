module Types.JString
  ( genJString
  ) where

import           Control.Lens             (( # ), _Wrapped)

import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range

import qualified Data.Vector              as V

import           Types.JChar              (genJChar)

import           Waargonaut.Types.JString (JString)

genJString
  :: Gen JString
genJString =
  (_Wrapped #) . V.fromList <$> Gen.list (Range.linear 0 1000) genJChar
