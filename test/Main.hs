{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           GHC.Word                    (Word8)

import           Control.Lens                (( # ), (^.), (^?), _2)
import qualified Control.Lens                as L
import           Control.Monad               (when)

import           Data.Either                 (isLeft)

import qualified Data.Scientific             as Sci

import Data.Functor.Contravariant ((>$<))
import           Data.Maybe                  (fromMaybe)
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text

import qualified Data.Text.Lazy              as TextL
import qualified Data.Text.Lazy.Builder      as TB

import qualified Data.ByteString             as BS

import qualified Data.Sequence               as S

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Natural                     (_Natural)

import           Data.Digit                  (HeXDigit)

import           Waargonaut                  (Json)
import qualified Waargonaut                  as W
import qualified Waargonaut.Types.CommaSep   as CommaSep

import           Waargonaut.Types.JChar      (JChar)
import qualified Waargonaut.Types.JChar      as JChar

import qualified Waargonaut.Types.JNumber    as JNumber
import qualified Waargonaut.Types.Whitespace as WS

import qualified Waargonaut.Decode           as D
import           Waargonaut.Decode.Error     (DecodeError)
import           Waargonaut.Decode.Internal  (CursorHistory' (..),
                                              ZipperMove (..), compressHistory)
import qualified Waargonaut.Encode           as E

import           Waargonaut.Generic          (mkDecoder, mkEncoder)

import qualified Types.CommaSep              as CS
import qualified Types.Common                as Common
import qualified Types.Json                  as J
import qualified Types.Whitespace            as WS

import qualified Decoder
import qualified Decoder.Laws
import qualified Encoder
import qualified Encoder.Laws
import qualified Json

encodeText
  :: Json
  -> Text
encodeText = TextL.toStrict
  . TB.toLazyText
  . W.waargonautBuilder WS.wsBuilder

decode
  :: Text
  -> Either DecodeError Json
decode =
  Common.parseText

prop_history_condense :: Property
prop_history_condense = property $ do
  n <- forAll $ Gen.int (Range.linear 1 10)
  m <- forAll $ Gen.int (Range.linear 1 10)

  let
    ixa = 1 :: Int
    ixb = 2
    mkCH = CursorHistory' . S.fromList
    mcA cn cm n' m' = mkCH [(cn (n' ^. _Natural), ixa), (cm (m' ^. _Natural), ixb)]
    mcB c x i = mkCH [(c (x ^. _Natural), i)]

  -- * [R n, R m]   = [R (n + m)]
  compressHistory (mcA R R n m) === mcB R (n + m) ixb

  -- * [L n, R m]   = [L (n + m)]
  compressHistory (mcA L L n m) === mcB L (n + m) ixb

  let
    rlch = compressHistory (mcA R L n m)
    lrch = compressHistory (mcA L R n m)
  when (n > m) $ do
    -- * [R n, L m]   = [R (n - m)] where n > m
    rlch === mcB R (n - m) ixa
    -- * [L n, R m]   = [L (n - m)] where n > m
    lrch === mcB L (n - m) ixa

  when (n < m) $ do
    -- * [R n, L m]   = [L (m - n)] where n < m
    rlch === mcB L (m - n) ixb
    -- * [L n, R m]   = [R (m - n)] where n < m
    lrch === mcB R (m - n) ixb

  -- * [DAt k, R n] = [DAt k]
  compressHistory (mkCH [(DAt "KeyName", ixa), (R (n ^. _Natural), ixb)]) === mkCH [(DAt "KeyName", ixa)]

prop_uncons_consCommaSep :: Property
prop_uncons_consCommaSep = property $ do
  cs <- forAll $ CS.genCommaSeparated WS.genWS Gen.bool
  let
    elems = (^. CommaSep._CommaSeparated . _2)

    cs' = do
      (e,xs) <- CommaSep.unconsCommaSep cs
      let trailing = fromMaybe (CommaSep.Comma, mempty) (fst e)
      elems $ CommaSep.consCommaSep (trailing, snd e) xs

  elems cs === cs'

prop_uncons_consCommaSepVal :: Property
prop_uncons_consCommaSepVal = property $ do
  cs <- forAll $ CS.genCommaSeparated WS.genEmptyWS Gen.bool
  let
    elems = (^. CommaSep._CommaSeparated . _2)

  elems cs === (elems . uncurry L.cons =<< L.uncons cs)

prop_jchar :: Property
prop_jchar = property $ do
  c <- forAll Gen.unicodeAll
  tripping c toJChar (fmap fromJChar)
  where
    fromJChar :: JChar HeXDigit -> Char
    fromJChar = (JChar._JChar #)

    toJChar :: Char -> Maybe (JChar HeXDigit)
    toJChar = (^? JChar._JChar)

prop_jnumber_scientific_prism :: Property
prop_jnumber_scientific_prism = property $ do
  sci <- forAll $ Sci.scientific
    <$> Gen.integral (Range.linear 0 maxI)
    <*> Gen.int Range.linearBounded

  tripping sci (JNumber._JNumberScientific #) (^? JNumber._JNumberScientific)
  where
    maxI :: Integer
    maxI = 2 ^ (32 :: Integer)

simpleDecodeWith :: D.Decoder L.Identity a -> TextL.Text -> Either (DecodeError, D.CursorHistory) a
simpleDecodeWith d = D.simpleDecode d Common.parseBS . Text.encodeUtf8 . TextL.toStrict

prop_tripping_int_list :: Property
prop_tripping_int_list = property $ do
  xs <- forAll . Gen.list (Range.linear 0 100) $ Gen.int (Range.linear 0 9999)
  tripping xs
    (E.simplePureEncodeNoSpaces (E.traversable E.int))
    (simpleDecodeWith (D.list D.int))

prop_tripping_image_record_generic :: Property
prop_tripping_image_record_generic = withTests 1 . property $
  Common.prop_generic_tripping mkEncoder mkDecoder Common.testImageDataType

prop_tripping_newtype_fudge_generic :: Property
prop_tripping_newtype_fudge_generic = withTests 1 . property $
  Common.prop_generic_tripping mkEncoder mkDecoder Common.testFudge

prop_tripping_maybe_bool_generic :: Property
prop_tripping_maybe_bool_generic = property $
  forAll (Gen.maybe Gen.bool) >>= Common.prop_generic_tripping mkEncoder mkDecoder

prop_tripping_int_list_generic :: Property
prop_tripping_int_list_generic = property $ do
  xs <- forAll . Gen.list (Range.linear 0 100) $ Gen.int (Range.linear 0 9999)
  Common.prop_generic_tripping mkEncoder mkDecoder xs

prop_tripping :: Property
prop_tripping = withTests 200 . property $
  forAll J.genJson >>= (\j -> tripping j encodeText decode)

prop_print_parse_print_id :: Property
prop_print_parse_print_id = withTests 200 . property $ do
  printedA <- forAll $ encodeText <$> J.genJson
  Right printedA === (encodeText <$> decode printedA)

prop_maybe_maybe :: Property
prop_maybe_maybe = withTests 1 . property $ do
  let
    n   = Nothing
    jn  = Just Nothing
    jjt = Just (Just True)
    jjf = Just (Just False)

  trippin' n
  trippin' jn
  trippin' jjt
  trippin' jjf
  where
    trippin' a = tripping a
      (E.simplePureEncodeNoSpaces enc)
      (simpleDecodeWith dec)

    enc = E.maybeOrNull' . E.mapLikeObj' . E.atKey' "boop"
      $ E.maybeOrNull' (E.mapLikeObj' (E.atKey' "beep" E.bool'))
      -- $ E.mapLikeObj (E.atKey "beep" (E.maybeOrNull E.bool))

    dec = D.maybeOrNull $ D.atKey "boop"
      $ D.maybeOrNull (D.atKey "beep" D.bool)
      -- $ D.atKey "beep" (D.maybeOrNull D.bool)

tripping_properties :: TestTree
tripping_properties = testGroup "Properties"
  [ testProperty "CommaSeparated: cons . uncons = id"                  prop_uncons_consCommaSep
  , testProperty "CommaSeparated (disregard WS): cons . uncons = id"   prop_uncons_consCommaSepVal
  , testProperty "Char -> JChar Digit -> Maybe Char = Just id"         prop_jchar
  , testProperty "Scientific -> JNumber -> Maybe Scientific = Just id" prop_jnumber_scientific_prism
  , testProperty "(Maybe (Maybe Bool))"                                prop_maybe_maybe
  , testProperty "[Int]"                                               prop_tripping_int_list
  , testProperty "[Int] (generic)"                                     prop_tripping_int_list_generic
  , testProperty "Maybe Bool (generic)"                                prop_tripping_maybe_bool_generic
  , testProperty "Image record (generic)"                              prop_tripping_image_record_generic
  , testProperty "Newtype with Options (generic)"                      prop_tripping_newtype_fudge_generic
  , testProperty "Condensing History"                                  prop_history_condense
  ]

parser_properties :: TestTree
parser_properties = testGroup "Parser Round-Trip"
  [ testProperty "parse . print = id"            prop_tripping
  , testProperty "print . parse . print = print" prop_print_parse_print_id
  ]

parsePrint :: Text -> Either DecodeError Text
parsePrint = fmap encodeText . decode

readTestFile :: FilePath -> IO Text
readTestFile fp = Text.readFile ("test/json-data" <> "/" <> fp)

testFile :: FilePath -> Assertion
testFile fp = do
  s <- readTestFile fp
  parsePrint s @?= Right s

testFileFailure :: FilePath -> Assertion
testFileFailure fp = do
  s <- readTestFile fp
  assertBool (fp <> " should fail to parse!") (isLeft $ parsePrint s)

unitTests :: TestTree
unitTests =
  testGroup "File Tests - (print . parse = id)" (toTest <$> fs)
  where
    toTest f = testCase f (testFile f)

    fs =
      [ "test1.json"
      , "test2.json"
      , "test3.json"
      , "test5.json"
      , "test7.json"
      , "numbers.json"
      ]

mishandlingOfCharVsUtf8Bytes :: TestTree
mishandlingOfCharVsUtf8Bytes = testCaseSteps "Mishandling of UTF-8 Bytes vs Haskell Char" $ \step -> do
  let
    valChar      = '\128'          :: Char
    valText      = "\128"          :: Text
    valStr       = [valChar]        :: String
    encVal       = "\"\128\""      :: Text
    valUtf8Bytes = [34,194,128,34] :: [Word8]

  step "Pack String to Text"
  Text.pack valStr @?= valText

  step "Encoder via Text"
  x <- TextL.toStrict <$> E.simpleEncodeNoSpaces E.text valText
  x @?= encVal

  step "encoder output ~ packed bytes"
  Text.encodeUtf8 x @?= BS.pack valUtf8Bytes

  step "Decoder via Text"
  y <- D.runDecode D.text Common.parseBS (D.mkCursor (Text.encodeUtf8 x))
  y @?= Right valText

regressionTests :: TestTree
regressionTests =
  testGroup "Regression Tests - Failure to parse = Success" (toTestFail <$> fs)
  where
    toTestFail (dsc, f) =
      testCase dsc (testFileFailure f)

    fs =
      [ ("[11 12 13] (test4.json)","test4.json")
      , ("{\"foo\":3\"bar\":4} (test6.json)", "test6.json")
      ]

main :: IO ()
main = defaultMain $ testGroup "Waargonaut All Tests"
  [ parser_properties
  , tripping_properties
  , unitTests
  , regressionTests
  , Json.jsonTests
  , mishandlingOfCharVsUtf8Bytes

  , Decoder.decoderTests
  , Encoder.encoderTests

  , Decoder.Laws.decoderLaws
  , Encoder.Laws.encoderLaws

  , testGroup "text gen - text e/d"
    [ testProperty "unicode"       $ p Gen.text Gen.unicode E.text D.text
    , testProperty "latin1"        $ p Gen.text Gen.latin1 E.text D.text
    , testProperty "ascii"         $ p Gen.text Gen.ascii E.text D.text
    ]
  , testGroup "bytestring gen - via text e/d"
    [ testProperty "unicode"       $ p Gen.utf8 Gen.unicode bsE bsD
    , testProperty "latin1"        $ p Gen.utf8 Gen.latin1 bsE bsD
    , testProperty "ascii"         $ p Gen.utf8 Gen.ascii bsE bsD
    ]
  ]
  where
    bsE = Text.decodeUtf8 >$< E.text
    bsD = Text.encodeUtf8 <$> D.text

    p :: ( Eq a
         , Show a
         )
      => (Range Int -> Gen Char -> Gen a)
      -> Gen Char
      -> E.Encoder' a
      -> D.Decoder L.Identity a
      -> Property
    p f g e d = property $ do
      inp <- forAll $ f (Range.linear 0 1000) g
      tripping inp (E.simplePureEncodeNoSpaces e) (simpleDecodeWith d)
