{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
--
-- This module contains the types and functions that power the 'Generic' functions for Waargonaut. Code
-- that writes the code so you don't have to.
--
module Waargonaut.Generic
  (
    -- * Rationale
    -- $rationale

    -- * Quick Start
    -- $quick

    -- * Tagged
    -- $tagged

    -- * GHC >= 8 Convenience
    -- $nice

    -- * TypeClasses
    JsonEncode (..)
  , JsonDecode (..)

    -- * Tag
  , GWaarg

    -- * Options
  , NewtypeName (..)
  , Options (..)
  , defaultOpts
  , trimPrefixLowerFirst

    -- * Creation
  , gEncoder
  , gDecoder

    -- * Reexports
  , module Data.Tagged
  , Generic (..)
  , HasDatatypeInfo (..)
  ) where

import           Generics.SOP

import           Control.Lens                  (findOf, folded, isn't, _Left)
import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (lift, throwError)
import           Control.Monad.Reader          (runReaderT)
import           Control.Monad.State           (modify)

import qualified Data.Char                     as Char
import           Data.Maybe                    (fromMaybe)

import           Data.List.NonEmpty            (NonEmpty)

import           Data.ByteString               (ByteString)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           Data.Scientific               (Scientific)

import           Data.Tagged
import qualified Data.Tagged                   as T

import           Waargonaut                    (Json)

import           Waargonaut.Encode             (Encoder, Encoder')
import qualified Waargonaut.Encode             as E


import           HaskellWorks.Data.Positioning (Count)

import           Waargonaut.Decode             (Decoder)
import qualified Waargonaut.Decode             as D

import           Waargonaut.Decode.Error       (DecodeError (..))
import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeResultT (..),
                                                runDecoderResultT)
import           Waargonaut.Decode.Types       (unDecodeResult)

-- $setup
-- >>> :set -XOverloadedStrings

-- $rationale
-- Although creating your 'Decoder's and 'Encoder's explicitly is the preferred way of utilising
-- Waargonaut. The 'Generic' mechanism within Haskell provides immense opportunity to reduce or
-- eliminate the need to write code. Given the mechanical nature of JSON this a benefit that cannot
-- be ignored.
--
-- There are two typeclasses provided, 'JsonEncode' and 'JsonDecode'. Each with a single function
-- that will generate a 'Encoder' or 'Decoder' for that type. Normally, typeclasses such as these
-- are only parameterised over the type that is to be encoded/decoded. Which is acceptable if there
-- is only ever a single possible way to encode or decode a value of that type. However this is
-- rarely the case, even with respect to strings or numbers.
--
-- To account for this, the 'JsonEncode' and 'JsonDecode' typeclasses require an additional type
-- parameter @ t @. This parameter allows you to differentiate between the alternative ways of
-- encoding or decoding a single type @ a @. This parameter is attached to the 'Encoder' or
-- 'Decoder' using the 'Tagged' newtype. Allowing the type system to help you keep track of them.
--

-- $quick
-- A quick example on how to use the Waargonaut 'Generic' functionality. We will use the following
-- type and let GHC and 'Generic' write our 'Encoder' and 'Decoder' for us.
--
-- @
-- data Image = Image
--   { _imageWidth    :: Int
--   , _imageHeight   :: Int
--   , _imageTitle    :: Text
--   , _imageAnimated :: Bool
--   , _imageIDs      :: [Int]
--   }
--   deriving (Eq, Show)
-- @
--
-- Ensure we have the required imports and language options:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import qualified GHC.Generic as GHC
-- import Waargonaut.Generic (Generic, HasDatatypeInfo, JsonEncode, JsonDecode, GWaarg)
-- @
--
-- Update our data type 'deriving' to have GHC to do the heavy lifting:
--
-- @
-- data Image = Image
--   ...
--   deriving (..., GHC.Generic)
-- @
--
-- Because Waargonaut uses the <https://hackage.haskell.org/package/generics-sop 'generics-sop'>
-- package to make the 'Generic' functions easier to write and maintain. We need two more instances,
-- note that we don't have to write these either. We can leave these empty and the default
-- implementations, courtesy of 'Generic', will handle it for us.
--
-- @
-- instance HasDatatypeInfo Image
-- instance Generic Image
-- @
--
-- Now we can define our 'JsonEncode' and 'JsonDecode' instances. We need to provide the @ t @
-- parameter. Assume we have no special requirements, so we can use the 'GWaarg' tag.
--
-- @
-- instance JsonEncode GWaarg Image
-- instance JsonDecode GWaarg Image
-- @
--
-- That's it! We can now use 'mkEncoder' and 'mkDecoder' to write the code for our @Image@ type.
-- These will be tagged with our 'GWaarg' phantom type parameter:
--
-- @
-- mkEncoder :: Applicative f => Tagged GWaarg (Encoder f Image)
-- mkDecoder :: Monad f       => Tagged GWaarg (Decoder f Image)
-- @
--
-- The encoding and decoding "runner" functions will require that you remove the tag. You can use
-- the 'untag' function for this. The next section will discuss the 'Tagged' type.
--
-- There is Template Haskell available that can write all of the 'Generic' deriving for you, see the
-- <https://hackage.haskell.org/package/generics-sop/docs/Generics-SOP-TH.html 'Generics.SOP.TH'>
-- module in the 'generics-sop' package for more. Given how little boilerplate code is required and
-- that the Template Haskell extension enforces a strict ordering of code within the file. It is not
-- the recommended solution. But I'm not your supervisor, I'm just a library.

-- $tagged
-- #tagged#
-- The 'Tagged' type comes from the <https://hackage.haskell.org/package/tagged 'tagged'> package.
-- It is a 'newtype' that provides a phantom type parameter. As well as having a several useful
-- typeclass instances and helpful functions already written for us.
--
-- When dealing with the 'Tagged' 'Encoder's and 'Decoder's there are two functions that are
-- particularly useful; 'untag', and 'proxy'.
--
-- The 'untag' function removes the tag from the inner type:
--
-- @
-- untag :: -- forall k (s :: k) b. Tagged s b -> b
-- @
--
-- When used with one of the 'Tagged' 'Generic' functions:
--
-- @
-- let e = mkEncoder :: Applicative f => Tagged GWaarg (Encoder f Image)
--
-- untag e :: Applicative f => Encoder f Image
-- @
--
-- The other function 'proxy', allows you to use 'mkEncoder' or 'mkDecoder' with the desired @ t @
-- parameter and then immediately remove the tag. This function requires the use of some @proxy@
-- that carries the same @ t @ of your instance:
--
-- @
-- proxy :: Tagged s a -> proxy s -> a
-- @
--
-- One way to utilise this function is in combination with 'Data.Proxy.Proxy' from @base@:
--
-- @
-- (proxy mkDecoder (Proxy :: Proxy GWaarg)) :: Monad f => Decoder f Image
-- @
--
-- This lets you skip the 'untag' step but without losing the safety of the 'Tagged' phantom type.
--

-- $nice
-- All of the techniques described above are explicit and will work in all versions of GHC that
-- Waargonaut supports. Should you be running a GHC that is version 8.0.1 or later, then you have
-- access to a language extension called <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications TypeApplications>.
--
-- This extension allows you to avoid much of the explicit type annotations described in Tagged
-- section of "Waargonaut.Generic#tagged". For example the 'proxy' function may be utilised like so:
--
-- @
-- (proxy mkDecoder (Proxy :: Proxy GWaarg)) :: Monad f => Decoder f Image
-- @
--
-- Becomes:
--
-- @
-- (proxy mkDecoder \@GWaarg) :: Monad f => Decoder f Image
-- @
--
-- You can also use the @TypeApplications@ directly on the 'mkEncoder' or 'mkDecoder' function:
--
-- @
-- mkEncoder \@GWaarg :: Applicative f => Tagged GWaarg (Encoder f Image)
-- mkDecoder \@GWaarg :: Monad f       => Tagged GWaarg (Decoder f Image)
-- @
--

-- | This is a provided tag that may be used for tagging the 'JsonEncode' and 'JsonDecode'
-- instances. You are encouraged to make your own tags for full control of your own instances.
data GWaarg

-- | The options we currently have for using the 'Generic' mechanism to handle 'newtype' values:
data NewtypeName

  -- | Discard the newtype wrapper and encode the inner value.
  --
  -- @
  -- newtype Foo = Foo Text
  --
  -- let x = Foo "Fred"
  -- @
  --
  -- Will be encoded as: @ "Fred" @
  --
  = Unwrap

  -- | Encode the newtype value as an object using the constructor as the "key".
  --
  -- @
  -- newtype Foo = Foo Text
  --
  -- let x = Foo "Fred"
  -- @
  --
  -- Will be encoded as: @ {"Foo":"Fred"} @
  --
  | ConstructorNameAsKey
  deriving (Show, Eq)

-- | The configuration options for creating 'Generic' encoder or decoder values.
data Options = Options
  { -- | When encoding/decoding a record type, this function will be used on the field names to
    -- determine how they will be encoded. Or what keys to look up on the JSON object when it is being
    -- decoded.
    _optionsFieldName           :: String -> String

    -- | How to handle 'newtype' values. See 'NewtypeName' for more info.
  , _optionsNewtypeWithConsName :: NewtypeName
  }

-- | Default options for 'Generic' functionality:
--
-- * Field names are left untouched: ('id')
-- * Newtype values are encoded as raw values: ('Unwrap')
--
defaultOpts :: Options
defaultOpts = Options id Unwrap

-- |
-- Helper function to alter record field names for encoding and decoding. Intended use is to be
-- given the prefix you would like to have removed and then included in the 'Options' for the
-- typeclass you are implementing.
--
-- A common use case when encoding Haskell record types is to remove a prefix and then lower-case
-- the first letter:
--
-- >>> trimPrefixLowerFirst "_image" "_imageHeight"
-- "height"
--
-- >>> trimPrefixLowerFirst "_image" "Height"
-- "Height"
--
-- >>> trimPrefixLowerFirst "_image" ""
-- ""
--
-- >>> trimPrefixLowerFirst "" "_imageHeight"
-- "_imageHeight"
--
trimPrefixLowerFirst :: Text -> String -> String
trimPrefixLowerFirst p n = maybe n f
  $ Text.uncons =<< Text.stripPrefix p (Text.pack n)
  where f (h',t') = Text.unpack $ Text.cons (Char.toLower h') t'

-- |
-- Encoding Typeclass for Waargonaut.
--
-- This type class is responsible for creating an 'Encoder' for the type of @ a @, differentiated
-- from the other possible instances of this typeclass for type @ a @ by the tag type @ t @.
--
-- To create a 'Tagged' 'Encoder' for the purposes of writing an instance your self, you need only
-- data constructor 'Tagged' from 'Data.Tagged'. It has been re-exported from this module.
--
-- @
-- instance JsonEncode GWaarg Foo where
--   mkEncoder = Tagged fooEncoderIWroteEarlier
-- @

class JsonEncode t a where
  mkEncoder :: Applicative f => Tagged t (Encoder f a)

  default mkEncoder
    :: ( Applicative f
       , Generic a
       , HasDatatypeInfo a
       , All2 (JsonEncode t) (Code a)
       )
    => Tagged t (Encoder f a)
  mkEncoder =
    gEncoder defaultOpts

instance JsonEncode t a                   => JsonEncode t (Maybe a)    where mkEncoder = E.maybeOrNull <$> mkEncoder
instance (JsonEncode t a, JsonEncode t b) => JsonEncode t (Either a b) where mkEncoder = E.either <$> mkEncoder <*> mkEncoder
instance (JsonEncode t a)                 => JsonEncode t [a]          where mkEncoder = E.list <$> mkEncoder
instance (JsonEncode t a)                 => JsonEncode t (NonEmpty a) where mkEncoder = E.nonempty <$> mkEncoder
instance JsonEncode t Text                                             where mkEncoder = Tagged E.text
instance JsonEncode t Int                                              where mkEncoder = Tagged E.int
instance JsonEncode t Scientific                                       where mkEncoder = Tagged E.scientific
instance JsonEncode t Bool                                             where mkEncoder = Tagged E.bool
instance JsonEncode t Json                                             where mkEncoder = Tagged E.json

-- |
-- Decoding Typeclass for Waargonaut
--
-- Responsible for creating a 'Decoder' for the type @ a @, differentiated from the other possible
-- instances of this typeclass for type @ a @ by the tag type @ t @.
--
-- To create a 'Tagged' 'Decoder' for the purposes of writing an instance your self, you need only
-- data constructor 'Tagged' from 'Data.Tagged'. It has been re-exported from this module.
--
-- @
-- instance JsonDecode GWaarg Foo where
--   mkDecoder = Tagged fooDecoderIWroteEarlier
-- @
--
class JsonDecode t a where
  mkDecoder :: Monad f => Tagged t (Decoder f a)

  default mkDecoder
    :: ( Monad f
       , Generic a
       , HasDatatypeInfo a
       , All2 (JsonDecode t) (Code a)
       ) => Tagged t (Decoder f a)
  mkDecoder =
    gDecoder defaultOpts

instance JsonDecode t a                   => JsonDecode t (Maybe a)    where mkDecoder = D.maybeOrNull <$> mkDecoder
instance (JsonDecode t a, JsonDecode t b) => JsonDecode t (Either a b) where mkDecoder = D.either <$> mkDecoder <*> mkDecoder
instance (JsonDecode t a)                 => JsonDecode t [a]          where mkDecoder = D.list <$> mkDecoder
instance (JsonDecode t a)                 => JsonDecode t (NonEmpty a) where mkDecoder = D.nonempty <$> mkDecoder
instance JsonDecode t Text                                             where mkDecoder = Tagged D.text
instance JsonDecode t Int                                              where mkDecoder = Tagged D.int
instance JsonDecode t Scientific                                       where mkDecoder = Tagged D.scientific
instance JsonDecode t Bool                                             where mkDecoder = Tagged D.bool
instance JsonDecode t Json                                             where mkDecoder = Tagged D.json

type JTag = String

data Tag
  = NoTag
  | Tag JTag
  deriving Show

data JsonInfo :: [*] -> * where
  JsonZero :: ConstructorName -> JsonInfo '[]
  JsonOne  :: Tag -> JsonInfo '[a]
  JsonMul  :: SListI xs => Tag -> JsonInfo xs
  JsonRec  :: SListI xs => Tag -> NP (K String) xs -> JsonInfo xs

modFieldName
  :: Options
  -> String
  -> Text
modFieldName opts =
 Text.pack . _optionsFieldName opts

inObj :: Encoder' a -> String -> Encoder' a
inObj en t = E.mapLikeObj' (E.atKey' (Text.pack t) en)

tagVal
  :: Applicative f
  => Tag
  -> f Json
  -> K (f Json) xs
tagVal  NoTag  v =
  K v
tagVal (Tag t) v =
  K $ E.asJson' (inObj E.json' t) <$> v

unTagVal
  :: Monad f
  => Tag
  -> Decoder f c
  -> D.JCurs
  -> D.DecodeResult f c
unTagVal NoTag   d =
  D.focus d
unTagVal (Tag n) d =
  D.down >=> D.fromKey (Text.pack n) d

jInfoFor
  :: forall xs.
     Options
  -> DatatypeName
  -> (ConstructorName -> Tag)
  -> ConstructorInfo xs
  -> JsonInfo xs
jInfoFor _ _ tag (Infix n _ _) = JsonMul (tag n)
jInfoFor _ _ tag (Constructor n) =
  case shape :: Shape xs of
    ShapeNil           -> JsonZero n
    ShapeCons ShapeNil -> JsonOne (tag n)
    _                  -> JsonMul (tag n)
jInfoFor opts _ tag (Record n fs) =
  JsonRec (tag n) (hliftA fname fs)
  where
    fname :: FieldInfo a -> K String a
    fname (FieldInfo name) = K $ _optionsFieldName opts name

jsonInfo
  :: forall a.
     ( HasDatatypeInfo a
     , SListI (Code a)
     )
  => Options
  -> Proxy a
  -> NP JsonInfo (Code a)
jsonInfo opts pa =
  case datatypeInfo pa of
    Newtype _ _ c  -> JsonOne (newtypename (constructorName c)) :* Nil
    ADT     _ n cs -> hliftA (jInfoFor opts n (tag cs)) cs
  where
    newtypename n = case _optionsNewtypeWithConsName opts of
      Unwrap               -> NoTag
      ConstructorNameAsKey -> Tag (_optionsFieldName opts n)

    tag :: NP ConstructorInfo (Code a) -> ConstructorName -> Tag
    tag (_ :* Nil) = const NoTag
    tag _          = Tag

-- |
-- Create a 'Tagged' 'Encoder' for type @ a @, tagged by @ t @, using the given 'Options'.
--
-- Combined with the 'defaultOpts' this is the default implementation of 'JsonEncode'.
--
-- Some examples:
--
-- @
-- instance JsonEncode GWaarg Image where
--   mkEncoder = gEncoder defaultOpts
-- @
--
-- @
-- instance JsonEncode GWaarg Image where
--   mkEncoder = gEncoder (defaultOpts { _optionsFieldName = trimPrefixLowerFirst "_image" })
-- @
--
gEncoder
  :: forall t a f.
     ( Generic a
     , Applicative f
     , HasDatatypeInfo a
     , All2 (JsonEncode t) (Code a)
     )
  => Options
  -> Tagged t (Encoder f a)
gEncoder opts = Tagged . E.encodeA $ \a -> hcollapse $ hcliftA2
  (Proxy :: Proxy (All (JsonEncode t)))
  (gEncoder' pjE pt opts)
  (jsonInfo opts (Proxy :: Proxy a))
  (unSOP $ from a)
  where
    pjE = Proxy :: Proxy (JsonEncode t)
    pt  = Proxy :: Proxy t

gEncoder'
  :: forall xs f t.
     ( All (JsonEncode t) xs
     , Applicative f
     )
  => Proxy (JsonEncode t)
  -> Proxy t
  -> Options
  -> JsonInfo xs
  -> NP I xs
  -> K (f Json) xs
gEncoder' _ _ _ (JsonZero n) Nil           =
  K (E.asJson (T.untag mkEncoder) (Text.pack n))

gEncoder' _ pT _ (JsonOne tag) (I a :* Nil) =
  tagVal tag $ E.asJson (T.proxy mkEncoder pT) a

gEncoder' p pT _ (JsonMul tag) cs           =
  tagVal tag . E.asJson (E.list E.json) . hcollapse $ hcliftA p ik cs
  where
    ik :: JsonEncode t x => I x -> K Json x
    ik = K . E.asJson' (T.proxy mkEncoder pT) . unI

gEncoder' p pT opts (JsonRec tag fields) cs    =
  tagVal tag . enc . hcollapse $ hcliftA2 p tup fields cs
  where
    tup :: JsonEncode t x => K String x -> I x -> K (Text, Json) x
    tup f a = K ( modFieldName opts (unK f)
                , E.asJson' (T.proxy mkEncoder pT) (unI a)
                )

    enc = pure . E.asJson' (E.keyValueTupleFoldable E.json)

-- |
-- Create a 'Tagged' 'Decoder' for type @ a @, tagged by @ t @, using the given 'Options'.
--
-- Combined with the 'defaultOpts' this is the default implementation of 'JsonEncode'.
--
-- Some examples:
--
-- @
-- instance JsonEncode GWaarg Image where
--   mkDecoder = gDecoder defaultOpts
-- @
--
-- @
-- instance JsonEncode GWaarg Image where
--   mkDecoder = gDecoder (defaultOpts { _optionsFieldName = trimPrefixLowerFirst "_image" })
-- @
--
gDecoder
  :: forall f a t.
     ( Generic a
     , HasDatatypeInfo a
     , All2 (JsonDecode t) (Code a)
     , Monad f
     )
  => Options
  -> Tagged t (Decoder f a)
gDecoder opts = Tagged $ D.Decoder $ \parseFn cursor ->
  to <$> gDecoderConstructor
           opts
           (Proxy :: Proxy (All (JsonDecode t)))
           parseFn
           cursor
           (jsonInfo opts (Proxy :: Proxy a))

gDecoderConstructor
  :: forall (xss :: [[*]]) f t.
     ( All2 (JsonDecode t) xss
     , Monad f
     )
  => Options
  -> Proxy (All (JsonDecode t))
  -> (ByteString -> Either DecodeError Json)
  -> D.JCurs
  -> NP JsonInfo xss
  -> DecodeResultT Count DecodeError f (SOP I xss)
gDecoderConstructor opts pJAll parseFn cursor ninfo =
  foldForRight . hcollapse $ hcliftA2 pJAll (mkGDecoder opts pJDec cursor) ninfo injs
  where
    pJDec = Proxy :: Proxy (JsonDecode t)

    err = Left ( ConversionFailure "Generic Decoder has failed, please file a bug."
               , CursorHistory' mempty
               )

    failure (e,h) = modify (const h) >> throwError e

    runDR = runDecoderResultT
      . flip runReaderT parseFn
      . unDecodeResult

    -- Pretty sure there is a better way to manage this, as my intuition about
    -- generic-sop says that I will only have one successful result for any
    -- given type. But I'm not 100% sure that this is actually the case.
    foldForRight :: [D.DecodeResult f (SOP I xss)] -> DecodeResultT Count DecodeError f (SOP I xss)
    foldForRight xs = (lift . sequence $ runDR <$> xs)
      >>= either failure pure . fromMaybe err . findOf folded (isn't _Left)

    injs :: NP (Injection (NP I) xss) xss
    injs = injections

mkGDecoder
  :: forall t (xss :: [[*]]) (xs :: [*]) f.
     ( All (JsonDecode t) xs
     , Monad f
     )
  => Options
  -> Proxy (JsonDecode t)
  -> D.JCurs
  -> JsonInfo xs
  -> Injection (NP I) xss xs
  -> K (D.DecodeResult f (SOP I xss)) xs
mkGDecoder opts pJDec cursor info (Fn inj) = K $ do
  val <- mkGDecoder2 opts pJDec cursor info
  SOP . unK . inj <$> hsequence (hcliftA pJDec aux val)
  where
    aux :: JsonDecode t x => K Count x -> D.DecodeResult f x
    aux (K rnk) = D.moveToRankN rnk cursor >>= D.focus (T.proxy mkDecoder (Proxy :: Proxy t))

mkGDecoder2
  :: forall t (xs :: [*]) f.
     ( All (JsonDecode t) xs
     , Monad f
    )
  => Options
  -> Proxy (JsonDecode t)
  -> D.JCurs
  -> JsonInfo xs
  -> D.DecodeResult f (NP (K Count) xs)
mkGDecoder2 _ _ cursor (JsonZero _) =
  Nil <$ unTagVal NoTag D.rank cursor

mkGDecoder2 _ _ cursor (JsonOne tag) =
  (\j -> K j :* Nil) <$> unTagVal tag D.rank cursor

mkGDecoder2 _ _ cursor (JsonMul tag) = do
  xs <- unTagVal tag (D.list D.rank) cursor
  maybe err pure (fromList xs)
  where
    err = throwError (ConversionFailure "Generic List Decode Failed")

mkGDecoder2 opts pJDec cursor (JsonRec tag fields) = do
  c' <- D.down cursor
  hsequenceK $ hcliftA pJDec (mapKK (decodeAtKey c')) fields
  where
    decodeAtKey c k = unTagVal tag (
      D.withCursor $ D.fromKey (modFieldName opts k) D.rank
      ) c
