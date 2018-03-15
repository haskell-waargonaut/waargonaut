module Types.JChar
  ( genJChar
  ) where

import           Hedgehog
import qualified Hedgehog.Gen           as Gen

import           Control.Lens           (preview)
import           Types.Common           (genHeXaDeCiMaLDigit, genWhitespace)

import           Data.Digit             (Digit)

import           Waargonaut.Types.JChar (HexDigit4 (..), JChar (..),
                                         JCharEscaped (..), JCharUnescaped (..),
                                         _JCharUnescaped)

genJChar :: Gen (JChar Digit)
genJChar = Gen.choice
  [ EscapedJChar <$> genJCharEscaped
  , UnescapedJChar <$> genJCharUnescaped
  ]

genJCharUnescaped :: Gen JCharUnescaped
genJCharUnescaped = Gen.just $ preview _JCharUnescaped <$> Gen.unicode

genJCharEscaped :: Gen (JCharEscaped Digit)
genJCharEscaped = do
  h4 <- genHex4
  ws <- genWhitespace
  Gen.element
    [ QuotationMark
    , ReverseSolidus
    , Solidus
    , Backspace
    , WhiteSpace ws
    , Hex h4
    ]

genHex4 :: Gen (HexDigit4 Digit)
genHex4 = HexDigit4
  <$> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit
