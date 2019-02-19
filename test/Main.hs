{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           GHC.Word                         (Word8)

import           Control.Lens                     ((^.), _2)
import qualified Control.Lens                     as L
import           Control.Monad                    (when)

import           Data.Either                      (isLeft)

import qualified Data.Scientific                  as Sci

import           Data.Functor.Contravariant       ((>$<))

import           Data.Maybe                       (fromMaybe)
import           Data.Semigroup                   ((<>))

import           Data.Char                        (ord)

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.IO                     as Text

import qualified Data.Text.Lazy                   as TextL

import qualified Data.Digit as Dig
import qualified Data.ByteString                  as BS
import qualified Data.Sequence                    as S

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Natural                          (_Natural)

import qualified Waargonaut.Types.CommaSep        as CommaSep

import qualified Waargonaut.Types.JChar           as JChar
import qualified Waargonaut.Types.JChar.HexDigit4 as Hex4

import qualified Waargonaut.Types.JNumber         as JNumber

import qualified Waargonaut.Decode                as D
import           Waargonaut.Decode.Error          (DecodeError)
import           Waargonaut.Decode.Internal       (CursorHistory' (..),
                                                   ZipperMove (..),
                                                   compressHistory)
import qualified Waargonaut.Encode                as E

import           Waargonaut.Generic               (mkDecoder, mkEncoder)

import qualified Types.CommaSep                   as CS
import qualified Types.Common                     as Common
import qualified Types.Json                       as J
import qualified Types.Whitespace                 as WS

import qualified Decoder
import qualified Decoder.Laws
import qualified Encoder
import qualified Encoder.Laws
import qualified Json

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
  tripping c JChar.charToJChar (fmap JChar.jCharToChar)

prop_jnumber_scientific_prism :: Property
prop_jnumber_scientific_prism = property $ do
  sci <- forAll $ Sci.scientific
    <$> Gen.integral (Range.linear 0 maxI)
    <*> Gen.int Range.linearBounded

  L.preview JNumber._JNumberScientific (L.review JNumber._JNumberScientific sci) === Just sci
  where
    maxI :: Integer
    maxI = 2 ^ (32 :: Integer)

prop_tripping_int_list :: Property
prop_tripping_int_list = property $ do
  xs <- forAll . Gen.list (Range.linear 0 100) $ Gen.int (Range.linear 0 9999)
  tripping xs
    (Common.encodeText (E.traversable E.int))
    (Common.simpleDecodeWith (D.list D.int))

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
  forAll J.genJson >>= (\j -> tripping j Common.encodeJsonText Common.decodeText)

prop_print_parse_print_id :: Property
prop_print_parse_print_id = withTests 200 . property $ do
  printedA <- forAll $ Common.encodeJsonText <$> J.genJson
  Right printedA === (Common.encodeJsonText <$> Common.decodeText printedA)

prop_builders_match :: Property
prop_builders_match = property $ do
  j <- forAll J.genJson

  let jt = Common.encodeJsonText j
      jb = Common.encodeBS j

  jt === Text.decodeUtf8 jb
  Text.encodeUtf8 jt === jb

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
      (Common.encodeText enc)
      (Common.simpleDecodeWith dec)

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
  , testProperty "HexDigit4 conversion"                                prop_char_heXDigit
  , testProperty "HexDigit4 upper-case hex chars regression"           prop_char_heXDigit_UpperCases
  , testProperty "Text & ByteString builders produce matching output"  prop_builders_match
  ]

charInAcceptableRange :: Char -> Bool
charInAcceptableRange c' = (ord c') >= 0x0 && (ord c') <= 0xffff

prop_char_heXDigit :: Property
prop_char_heXDigit = property $ do
  c <- forAll Gen.unicode

  let (anno, expect) = if charInAcceptableRange c
        then ("Char in valid range", Just c)
        else ("Char out of valid range", Nothing)

  annotate anno
  fmap Hex4.hexDigit4ToChar (Hex4.charToHexDigit4 c) === expect

prop_char_heXDigit_UpperCases :: Property
prop_char_heXDigit_UpperCases = property $ do
  c <- forAll $ Gen.filter charInAcceptableRange Gen.unicode

  let hex4 = Hex4.charToHexDigit4 c

  annotate "Generated Char should be in acceptable range!"
  hd <- maybe failure (pure . fmap ucHeX) hex4

  annotate "All upper-case hexdigits shouldn't affect conversion"
  Hex4.hexDigit4ToChar hd === c

  annotate "Conversion property should be maintained"
  fmap Hex4.hexDigit4ToChar hex4 === Just c
  where
    ucHeX Dig.HeXDigita = Dig.HeXDigitA
    ucHeX Dig.HeXDigitb = Dig.HeXDigitB
    ucHeX Dig.HeXDigitc = Dig.HeXDigitC
    ucHeX Dig.HeXDigitd = Dig.HeXDigitD
    ucHeX Dig.HeXDigite = Dig.HeXDigitE
    ucHeX Dig.HeXDigitf = Dig.HeXDigitF
    ucHeX d             = d

parser_properties :: TestTree
parser_properties = testGroup "Parser Round-Trip"
  [ testProperty "parse . print = id"            prop_tripping
  , testProperty "print . parse . print = print" prop_print_parse_print_id
  ]

parsePrint :: Text -> Either (DecodeError, D.CursorHistory) Text
parsePrint = fmap Common.encodeJsonText . Common.decodeText

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
      , "backslash128.json"
      ]

mishandlingOfCharVsUtf8Bytes :: TestTree
mishandlingOfCharVsUtf8Bytes = testCaseSteps "Mishandling of UTF-8 Bytes vs Haskell Char" $ \step -> do
  let
    valChar      = '\128'          :: Char
    valText      = "\128"          :: Text
    valStr       = [valChar]       :: String
    encVal       = "\"\128\""      :: Text
    valBytes = [34,194,128,34] :: [Word8]

    testFilePath = "test/json-data/mishandling.json"

  step "Pack String to Text"
  Text.pack valStr @?= valText

  step "Encoder via Text"
  let x = TextL.toStrict $ Common.encodeText E.text valText
  x @?= encVal

  step "Create JSON file"
  Text.writeFile testFilePath x

  step "encoder output ~ packed bytes"
  Text.encodeUtf8 x @?= BS.pack valBytes

  step "Decode file input"
  decodedFile <- Common.parseText D.text <$> Text.readFile testFilePath
  decodedFile @?= Right valText

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
  , Json.jsonPrisms
  , mishandlingOfCharVsUtf8Bytes

  , Decoder.decoderTests
  , Encoder.encoderTests

  , Decoder.Laws.decoderLaws
  , Encoder.Laws.encoderLaws

  , testGroup "text gen - text encoder/decoder"
    [ testProperty "unicode"       $ p Gen.text Gen.unicode E.text D.text
    , testProperty "latin1"        $ p Gen.text Gen.latin1 E.text D.text
    , testProperty "ascii"         $ p Gen.text Gen.ascii E.text D.text
    ]
  , testGroup "bytestring gen - via text encoder/decoder"
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
      tripping inp (Common.encodeText e) (Common.simpleDecodeWith d)
