{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
-- | Welcome to Waargonaut, we hope you enjoy your stay.
--
-- The handling of JSON is managed using the 'Waargonaut.Decode.Decoder' and
-- 'Waargonaut.Encode.Encoder' types, these are not typeclasses but data structures. As such you're
-- able to pass them around as values, manipulate or create them at runtime. This allows you to have
-- one data type, but several decoding and encoding techniques to match your requirements. You don't
-- have to pile on the newtypes or manage orphan instances.
--
module Waargonaut
  ( -- * Simple Decode
    -- $basicdecode

    -- * Simple Encode
    -- $basicencode

    -- * Types
    Json (..)
  , JType (..)

    -- * Parser / Builder
  , parseWaargonaut
  , waargonautBuilder

    -- * Prisms
  , _ByteStringJson
  , _TextJson
  , _Number
  , _String
  , _Bool
  , _ArrayOf
  , _ObjLazyHashMapOf
  , _Null

  ) where

import           Prelude                         (Bool, Show)

import           Control.Applicative             (liftA2)
import           Control.Category                ((.))
import           Control.Error.Util              (note)
import           Control.Lens                    (Prism', folded, preview,
                                                  prism, review, (^..), (^?),
                                                  _1, _2, _Wrapped)
import           Control.Monad                   (Monad)
import           Data.Foldable                   (foldl)
import           Data.Function                   (const, ($))
import           Data.Functor                    (fmap, (<$))
import           Data.Scientific                 (Scientific)

import           Data.Bifunctor                  (first)
import           Data.Either                     (Either (..))
import           Data.Maybe                      (Maybe (..), maybe)
import           Data.Monoid                     (mempty)
import           Data.Traversable                (traverse)

import           Text.Parser.Char                (CharParsing)

import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as TL

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as BL

import           Data.HashMap.Lazy               (HashMap)
import qualified Data.HashMap.Lazy               as HM

import qualified Waargonaut.Types.JObject.JAssoc as JA

import qualified Waargonaut.Types.CommaSep       as CS
import           Waargonaut.Types.JString        (_JStringText)

import           Waargonaut.Types.JNumber        (_JNumberScientific)
import           Waargonaut.Types.Json           (AsJType (..), JType (..),
                                                  Json (..), parseWaargonaut)

import           Waargonaut.Encode.Builder       (waargonautBuilder)

import qualified Waargonaut.Decode               as D
import qualified Waargonaut.Encode               as E

_ByteStringJson
  :: ( CharParsing g
     , Monad g
     , Show e
     )
  => (forall a. g a -> ByteString -> Either e a)
  -> Prism' ByteString Json
_ByteStringJson pf = prism
  (BL.toStrict . E.simplePureEncodeByteString E.json)
  (\b -> first (const b) $ D.pureDecodeFromByteString pf D.json b)

-- | Specialised to 'BS.ByteString'
_TextJson
  :: ( CharParsing g
     , Monad g
     , Show e
     )
  => (forall a. g a -> Text -> Either e a)
  -> Prism' Text Json
_TextJson pf = prism
  (TL.toStrict . E.simplePureEncodeText E.json)
  (\b -> first (const b) $ D.pureDecodeFromText pf D.json b)

-- | 'Prism'' between some 'Json' and a 'Scientific' value
_Number  :: Prism' Json Scientific
_Number = prism
  (review _JNum . (,mempty) . review _JNumberScientific)
  (\j -> note j $ j ^? _JNum . _1 . _JNumberScientific)

-- | Not a lawful 'Prism'' between some 'Json' and a 'Text' value
_String :: Prism' Json Text
-- _String = prism (review _JStr . (,mempty) . review _JStringText) (\j -> note j $ j ^? _JStr . _1 . _JStringText)
_String = prism (E.asJson' E.text) (\j -> note j $ j ^? _JStr . _1 . _JStringText)

-- | 'Prism'' between some 'Json' and a '()' value
_Null :: Prism' Json ()
_Null = prism (E.asJson' E.null) (\n -> note n $ () <$ n ^? _JNull)

-- | 'Prism'' between some 'Json' and a 'Bool' value
_Bool :: Prism' Json Bool
_Bool = prism (E.asJson' E.bool) (\j -> note j $ j ^? _JBool . _1)

-- | 'Prism'' between some 'Json' and an array of something given the provided 'Prism''
-- _ArrayOf :: Prism' Json x -> Prism' Json [x]
_ArrayOf :: Prism' Json x -> Prism' Json [x]
_ArrayOf elemT = prism fromList' toList'
  where
    fromList' = E.asJson' (E.list E.json) . fmap (review elemT)

    toList' r = maybe (Left r) (Right . (^.. folded . elemT))
      $ preview (_JArr . _1 . _Wrapped) r

_ObjLazyHashMapOf :: Prism' Json x -> Prism' Json (HashMap Text x)
_ObjLazyHashMapOf _Value = prism toJ fromJ
  where
    toJ = E.asJson' (E.keyValueTupleFoldable E.json) . HM.toList
      . fmap (review _Value)

    _ObjBits = _JObj . _1 . _Wrapped . CS._CommaSeparated . _2

    addToMap m (k,v) = HM.insert k v m

    toVals el = liftA2 (,)
      (preview (JA.jsonAssocKey . _JStringText) el)
      (preview (JA.jsonAssocVal . _Value) el)

    fromJ j = case preview _ObjBits j of
      Nothing         -> Left j
      Just Nothing    -> Right HM.empty
      Just (Just els) -> maybe (Left j) (Right . foldl addToMap HM.empty) $ traverse toVals els

-- $basicdecode
--
-- We will work through a basic example, using the following type:
--
-- @
-- data Person = Person
--   { _personName                    :: Text
--   , _personAge                     :: Int
--   , _personAddress                 :: Text
--   , _personFavouriteLotteryNumbers :: [Int]
--   }
--   deriving (Eq, Show)
-- @
--
-- Expect the following JSON as input:
--
-- @
-- { \"name\":    \"Krag\"
-- , \"age\":     88
-- , \"address\": \"Red House 4, Three Neck Lane, Greentown.\"
-- , \"numbers\": [86,3,32,42,73]
-- }
-- @
--
-- We'll need to import the 'Decode' module. You may of course use whatever import scheme you like,
-- I prefer this method:
--
-- @
-- import Waargonaut.Decode (Decoder)
-- import qualified Waargonaut.Decode as D
-- @
--
-- The 'Waargonaut.Decode.Decoder' is based upon a data structure called a 'zipper'. This allows us
-- to move around the JSON structure using arbitrary movements. Such as
-- 'Waargonaut.Decode.moveRight1' to move from a key on an object to the value at that key. Or
-- 'Waargonaut.Decode.down' to move into the first element of an array or object. Waargonaut
-- provides a suite of these functions to move around and dissect the JSON input.
--
-- This zipper is combined with a 'StateT' transformer that maintains a history of your movements.
-- So if the JSON input is not as your 'Waargonaut.Decode.Decoder' expects you are given a complete
-- path to where things went awry.
--
-- Decoding a JSON value is done by moving the cursor to specific points of interest, then focusing
-- on that point with a 'Waargonaut.Decode.Decoder' of the desired value.
--
-- NB: The 'Monad' constraint is provided as a flexibility for more interesting and nefarious uses
-- of 'Waargonaut.Decode.Decoder'.
--
-- Here is the 'Waargonaut.Decode.Decoder' for our 'Person' data type. It will help to turn on the
-- 'OverloadedStrings' language pragma as these functions expect 'Data.Text.Text' input.
--
-- @
-- personDecoder :: Monad f => Decoder f Person
-- personDecoder = D.withCursor $ \\c -> do
--   o     <- D.down c
--   name  <- D.fromKey "name" D.text o
--   age   <- D.fromKey "age" D.int o
--   addr  <- D.fromKey "address" D.text o
--   lotto <- D.fromKey "numbers" (D.list D.int) o
--   pure $ Person name age addr lotto
-- @
--
-- The 'Waargonaut.Decode.withCursor' function provides our cursor: 'c'. We then move
-- 'Waargonaut.Decode.down' into the JSON object. The reasons for this are:
--
-- * The initial cursor position is always at the very beginning of the input. On freshly indexed
--   JSON input, using our example, the cursor will be at:
--
-- @
-- \<cursor\>{ \"name\": \"Krag\"
--         , \"age\": 88
--         ...
-- @
--
-- * Because of the above reason, our decoder makes an assumption about the placement of the cursor
--   on the JSON input. This sort of assumption is reasonable for reasons we will go over later.
--
-- The cursor output from 'Waargonaut.Decode.down' will located here:
--
-- @
-- { \<cursor\>\"name\": \"Krag\"
--   , \"age\": 88
--   ...
-- @
--
-- Then we use one of the helper functions, 'Waargonaut.Decode.fromKey' to find the "key - value"
-- pair that we're interested in and decode it for us:
--
-- @
-- fromKey :: Monad f => Text -> Decoder f b -> JCurs -> DecodeResult f b
-- @
--
-- We could also write this 'Waargonaut.Decode.Decoder' as:
--
-- @
-- personDecoder2 :: Monad f => Decoder f Person
-- personDecoder2 = Person
--   \<$> D.atKey "name" D.text
--   \<*> D.atKey "age" D.int
--   \<*> D.atKey "address" D.text
--   \<*> D.atKey "numbers" (D.list D.int)
-- @
--
-- Using the 'Waargonaut.Decode.atKey' function which tries to handle those basic movements for us
-- and has those assumptions included. Very useful for when the JSON input closely mirrors your data
-- structure.
--
-- @
-- atKey :: Monad f => Text -> Decoder f a -> Decoder f a
-- @
--
-- The next part is being able to apply our 'Waargonaut.Decode.Decoder' to some input. Assuming we
-- have some input 'in'. We want to pass it through our 'personDecoder' for a result. Waargonaut uses
-- the <https://hackage.haskell.org/package/parsers parsers> package to define its parser. This
-- allows you to choose your own favourite parsing library to do the heavy lifting. Provided it
-- implements the right typeclasses from 'parsers'.
--
-- To apply a 'Waargonaut.Decode.Decoder' to some input you will need one of the
-- decoder running functions from 'Waargonaut.Decode'. There are a few different
-- functions provided for some of the common input text-like types.:
--
-- @
-- decodeFromByteString
--   :: ( CharParsing f
--      , Monad f
--      , Monad g
--      , Show e
--      )
--   => (forall a. f a -> ByteString -> Either e a)
--   -> Decoder g x
--   -> ByteString
--   -> g (Either (DecodeError, CursorHistory) x)
-- @
--
--
-- As well as a parsing function from your parsing library of choice, that also
-- has an implementation of the 'CharParsing' typeclass from 'parsers'. We will
-- use 'attoparsec' in the examples below.
--
-- @
-- import qualified Data.Attoparsec.ByteString as AB
-- @
--
-- @
-- decodeFromByteString AB.parseOnly personDecode inp
-- @
--
-- Which will run the 'personDecode' 'Waargonaut.Decode.Decoder' using the parsing function
-- (@AB.parseOnly@), starting at the cursor from the top of the 'inp' input.
--
-- Again the 'Monad' constraint is there so that you have more options available for utilising the
-- 'Waargonaut.Decode.Decoder' in ways we haven't thought of.
--
-- Or if you don't need the 'Monad' constraint then you may use 'Waargonaut.Decode.pureDecodeFromByteString'.
-- This function specialises the 'Monad' constraint to 'Data.Functor.Identity'.:
--
-- @
-- pureDecodeFromByteString
--   :: ( Monad f
--      , CharParsing f
--      , Show e
--      )
--   => (forall a. f a -> ByteString -> Either e a)
--   -> Decoder Identity x
--   -> ByteString
--   -> Either (DecodeError, CursorHistory) x
-- @
--
-- @
-- pureDecodeFromByteString AB.parseOnly personDecode inp
-- @
--

-- $basicencode
--
-- To create an 'Waargonaut.Encode.Encoder' for our 'Person' record, we will encode it as a "map
-- like object", that is we have decided that there are no duplicate keys allowed. We can then use
-- the following functions to build up the structure we want:
--
-- @
-- mapLikeObj
--   :: ( AsJType Json ws a
--      , Semigroup ws         -- This library supports GHC 7.10.3 and 'Semigroup' wasn't a superclass of 'Monoid' then.
--      , Monoid ws
--      , Applicative f
--      )
--   => (i -> MapLikeObj ws a -> MapLikeObj ws a)
--   -> Encoder f i
-- @
--
-- And:
--
-- @
-- atKey
--   :: ( At t
--      , IxValue t ~ Json
--      , Applicative f
--      )
--   => Index t
--   -> Encoder f a
--   -> a
--   -> t
--   -> f t
-- @
--
-- These types may seem pretty wild, but their usage is mundane. The 'Waargonaut.Encode.mapLikeObj'
-- function is used when we want to encode some particular type 'i' as a JSON object. In such a way
-- as to prevent duplicate keys from appearing. The 'Waargonaut.Encode.atKey' function is designed
-- such that it can be composed with itself to build up an object with multiple keys.
--
-- @
-- import Waargonaut.Encode (Encoder)
-- import qualified Waargonaut.Encode as E
-- @
--
-- @
-- personEncoder :: Applicative f => Encoder f Person
-- personEncoder = E.mapLikeObj $ \\p ->
--   E.atKey' \"name\" E.text (_personName p) .
--   E.atKey' \"age\" E.int (_personAge p) .
--   E.atKey' \"address\" E.text (_personAddress p) .
--   E.atKey' \"numbers\" (E.list E.int) (_personFavouriteLotteryNumbers p)
-- @
--
-- The JSON RFC leaves the handling of duplicate keys on an object as a choice. It is up to the
-- implementor of a JSON handling package to decide what they will do. Waargonaut passes on this
-- choice to you. In both encoding and decoding, the handling of duplicate keys is up to you.
-- Waargonaut provides functionality to support /both/ use cases.
--
-- To then turn these values into JSON output:
--
-- @
-- simpleEncodeText         :: Applicative f => Encoder f a -> a -> f Text
-- simpleEncodeTextNoSpaces :: Applicative f => Encoder f a -> a -> f Text
--
-- simpleEncodeByteString         :: Applicative f => Encoder f a -> a -> f ByteString
-- simpleEncodeByteStringNoSpaces :: Applicative f => Encoder f a -> a -> f ByteString
-- @
--
-- Or
--
-- @
-- simplePureEncodeText         :: Encoder' a -> a -> Text
-- simplePureEncodeTextNoSpaces :: Encoder' a -> a -> Text
--
-- simplePureEncodeByteString         :: Encoder' a -> a -> ByteString
-- simplePureEncodeByteStringNoSpaces :: Encoder' a -> a -> ByteString
-- @
--
-- The latter functions specialise the 'f' to be 'Data.Functor.Identity'.
--
-- Then, like the use of the 'Waargonaut.Decode.Decoder' you select the 'Waargonaut.Encode.Encoder'
-- you wish to use and run it against a value of a matching type:
--
-- @
-- simplePureEncodeTextNoSpaces personEncoder (Person \"Krag\" 33 \"Red House 4, Three Neck Lane, Greentown.\" [86,3,32,42,73])
-- =
-- "{\"name\":\"Krag\",\"age\":88,\"address\":\"Red House 4, Three Neck Lane, Greentown.\",\"numbers\":[86,3,32,42,73]}"
-- @
--
