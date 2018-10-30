{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Waargonaut.Generic
  (
    -- * TypeClasses
    JsonEncode (..)
  , JsonDecode (..)

    -- * Tag
  , GWaarg

    -- * Options
  , NewtypeName (..)
  , Options (..)
  , defaultOpts

    -- * Creation
  , gEncoder
  , gDecoder

  ) where

import           Generics.SOP

import           Control.Lens                  (findOf, folded, isn't, _Left)
import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (lift, throwError)
import           Control.Monad.State           (modify)

import           Data.Functor.Identity         (Identity, runIdentity)

import           Data.Maybe                    (fromMaybe)

import           Data.List.NonEmpty            (NonEmpty)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import qualified Data.Map                      as Map
import           Data.Scientific               (Scientific)

import           Data.Tagged                   (Tagged (..))
import qualified Data.Tagged                   as T

import           Waargonaut                    (Json)

import           Waargonaut.Encode             (Encoder, Encoder')
import qualified Waargonaut.Encode             as E


import           HaskellWorks.Data.Positioning (Count)

import           Waargonaut.Decode             (Decoder)
import qualified Waargonaut.Decode             as D

import           Waargonaut.Decode.Error       (DecodeError (..))
import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeResultT (..))

-- | This is a provided tag that can be used for tagging the 'JsonEncode' and 'JsonDecode' instances.
data GWaarg

-- | The two options we currently have for using the 'Generic' mechanism to handle 'newtype' values:
data NewtypeName
  -- | Discard the newtype wrapper and encode the inner value.
  --
  -- @
  -- newtype Foo = Foo Text
  --
  -- let x = Foo "Fred"
  -- @
  --
  -- Will be encoded as: "Fred"
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
  -- Will be encoded as: "{\"foo\":\"Fred\"}"
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
-- * Field names are left untouched (@id@)
-- * Newtype values are encoded as raw values (@Unwrap@)
--
defaultOpts :: Options
defaultOpts = Options id Unwrap

class JsonEncode t a where
  mkEncoder :: Applicative f => Tagged t (Encoder f a)

  default mkEncoder :: (Applicative f, Generic a, HasDatatypeInfo a, All2 (JsonEncode t) (Code a)) => Tagged t (Encoder f a)
  mkEncoder = gEncoder defaultOpts

instance JsonEncode t a                   => JsonEncode t (Maybe a)    where mkEncoder = E.maybeOrNull <$> mkEncoder
instance (JsonEncode t a, JsonEncode t b) => JsonEncode t (Either a b) where mkEncoder = E.either <$> mkEncoder <*> mkEncoder
instance (JsonEncode t a)                 => JsonEncode t [a]          where mkEncoder = E.list <$> mkEncoder
instance (JsonEncode t a)                 => JsonEncode t (NonEmpty a) where mkEncoder = E.nonempty <$> mkEncoder
instance JsonEncode t Text                                             where mkEncoder = Tagged E.text
instance JsonEncode t Int                                              where mkEncoder = Tagged E.int
instance JsonEncode t Scientific                                       where mkEncoder = Tagged E.scientific
instance JsonEncode t Bool                                             where mkEncoder = Tagged E.bool

class JsonDecode t a where
  mkDecoder :: Monad f => Tagged t (Decoder f a)
  default mkDecoder :: (Monad f, Generic a, HasDatatypeInfo a, All2 (JsonDecode t) (Code a)) => Tagged t (Decoder f a)
  mkDecoder = gDecoder defaultOpts

instance JsonDecode t a                   => JsonDecode t (Maybe a)    where mkDecoder = D.maybeOrNull <$> mkDecoder
instance (JsonDecode t a, JsonDecode t b) => JsonDecode t (Either a b) where mkDecoder = D.either <$> mkDecoder <*> mkDecoder
instance (JsonDecode t a)                 => JsonDecode t [a]          where mkDecoder = D.list <$> mkDecoder
instance (JsonDecode t a)                 => JsonDecode t (NonEmpty a) where mkDecoder = D.nonempty <$> mkDecoder
instance JsonDecode t Text                                             where mkDecoder = Tagged D.text
instance JsonDecode t Int                                              where mkDecoder = Tagged D.int
instance JsonDecode t Scientific                                       where mkDecoder = Tagged D.scientific
instance JsonDecode t Bool                                             where mkDecoder = Tagged D.bool

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
  K $ runIdentity . E.runEncoder (inObj E.json' t) <$> v

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
  K (E.runEncoder (T.untag mkEncoder) (Text.pack n))

gEncoder' _ pT _ (JsonOne tag) (I a :* Nil) =
  tagVal tag $ E.runEncoder (T.proxy mkEncoder pT) a

gEncoder' p pT _ (JsonMul tag) cs           =
  tagVal tag . E.runEncoder (E.list E.json) . hcollapse $ hcliftA p ik cs
  where
    ik :: JsonEncode t x => I x -> K Json x
    ik = K . runIdentity . E.runEncoder (T.proxy mkEncoder pT) . unI

gEncoder' p pT opts (JsonRec tag fields) cs    =
  tagVal tag . enc . hcollapse $ hcliftA2 p tup fields cs
  where
    tup :: JsonEncode t x => K String x -> I x -> K (Text, Json) x
    tup f a = K ( modFieldName opts (unK f)
                , runIdentity $ E.runEncoder (T.proxy mkEncoder pT) (unI a)
                )

    enc = pure . E.runPureEncoder (E.keyValueTupleFoldable E.json)

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
  -> D.ParseFn
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

    -- Pretty sure there is a better way to manage this, as my intuition about
    -- generic-sop says that I will only have one successful result for any
    -- given type. But I'm not 100% sure that this is actually the case.
    foldForRight :: [D.DecodeResult f (SOP I xss)] -> DecodeResultT Count DecodeError f (SOP I xss)
    foldForRight xs = (lift . sequence $ D.runDecodeResult parseFn <$> xs)
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
