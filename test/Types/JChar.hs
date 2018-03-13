module Types.JChar where

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Types.Common           (genDigit)

import           Waargonaut.Types.JChar (HexDigit4 (..), JChar (..),
                                         JCharEscaped (..), JCharUnescaped (..),
                                         jCharBuilder, parseJChar)

genJChar :: Gen (JChar Digit)
genJChar = Gen.choice
  [ EscapedJChar <$> genJCharEscaped
  , UnescapedJChar <$> genJCharUnescaped
  ]

genJCharUnescaped :: Gen JCharUnescaped
genJCharUnescaped = JCharUnescaped <$> Gen.unicodeAll

genJCharEscaped :: Gen (JCharEscaped Digit)
genJCharEscaped = do
  h4 <- genHex4
  Gen.element
    [ QuotationMark
    , ReverseSolidus
    , Solidus
    , Backspace
    , FormFeed
    , LineFeed
    , CarriageReturn
    , Tab
    , Hex h4
    ]

genHex4 :: Gen (HexDigit4 Digit)
genHex4 = HexDigit4 <$> genDigit <*> genDigit <*> genDigit <*> genDigit
