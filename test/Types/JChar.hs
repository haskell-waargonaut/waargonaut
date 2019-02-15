module Types.JChar
  ( genJChar
  , genHex4
  , genHex4Lower
  , genJCharEscaped
  , genJCharUnescaped
  ) where

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen

import           Control.Lens                     (preview)
import           Types.Common                     (genHeXaDeCiMaLDigit,
                                                   genHexadecimalDigitLower,
                                                   genWhitespace)

import           Data.Digit                       (HeXDigit, HexDigit)

import           Waargonaut.Types.JChar           (JChar (..))
import           Waargonaut.Types.JChar.Escaped   (Escaped (..))
import           Waargonaut.Types.JChar.HexDigit4 (HexDigit4 (..))
import           Waargonaut.Types.JChar.Unescaped (AsUnescaped (..), Unescaped)

genJChar :: Gen (JChar HeXDigit)
genJChar = Gen.choice
  [ EscapedJChar <$> genJCharEscaped
  , UnescapedJChar <$> genJCharUnescaped
  ]

genJCharUnescaped :: Gen Unescaped
genJCharUnescaped = Gen.just $ preview _Unescaped <$> Gen.unicode

genJCharEscaped :: Gen (Escaped HeXDigit)
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

genHex4 :: Gen (HexDigit4 HeXDigit)
genHex4 = HexDigit4
  <$> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit
  <*> genHeXaDeCiMaLDigit

genHex4Lower :: Gen (HexDigit4 HexDigit)
genHex4Lower = HexDigit4
  <$> genHexadecimalDigitLower
  <*> genHexadecimalDigitLower
  <*> genHexadecimalDigitLower
  <*> genHexadecimalDigitLower
