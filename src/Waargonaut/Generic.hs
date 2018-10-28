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

    -- * Wrappers
  , GJsonEncoder (..)
  , GJsonDecoder (..)
  , generaliseGJEncoder
  , generaliseGJDecoder

    -- * Options
  , NewtypeName (..)
  , Options (..)
  , defaultOpts

    -- *  Creation
  , gEncoder
  , gDecoder

    -- * Helpers
  , wGJEnc1
  , wGJEnc2
  , wGJDec1
  , wGJDec2

  ) where

import           Generics.SOP

import           Control.Lens                  (findOf, folded, isn't, _Left)
import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (lift, throwError)
import           Control.Monad.State           (modify)

import           Data.Functor.Contravariant    (Contravariant (..))
import           Data.Functor.Identity         (Identity, runIdentity)

import           Control.Monad.Morph           (MFunctor (..))

import           Data.Maybe                    (fromMaybe)

import           Data.List.NonEmpty            (NonEmpty)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import qualified Data.Map                      as Map
import           Data.Scientific               (Scientific)

import           Waargonaut                    (Json)

import           Waargonaut.Encode             (Encoder, Encoder')
import qualified Waargonaut.Encode             as E


import           HaskellWorks.Data.Positioning (Count)

import           Waargonaut.Decode             (Decoder)
import qualified Waargonaut.Decode             as D

import           Waargonaut.Decode.Error       (DecodeError (..))
import           Waargonaut.Decode.Internal    (CursorHistory' (..),
                                                DecodeResultT (..))

data GWaarg

data NewtypeName
  = Unwrap
  | ConstructorNameAsKey
  deriving (Show, Eq)

data Options = Options
  { _optionsFieldName           :: String -> String
  , _optionsNewtypeWithConsName :: NewtypeName
  }

defaultOpts :: Options
defaultOpts = Options id Unwrap

newtype GJsonEncoder t f a = GJEnc
  { unGJEnc :: Encoder f a
  }

instance Contravariant (GJsonEncoder t f) where
  contramap f (GJEnc e) = GJEnc (contramap f e)

instance MFunctor (GJsonEncoder t) where
  hoist nat (GJEnc e) = GJEnc (hoist nat e)

generaliseGJEncoder :: Monad f => GJsonEncoder t Identity a -> GJsonEncoder t f a
generaliseGJEncoder = GJEnc . E.generaliseEncoder' . unGJEnc

wGJEnc1
  :: (Encoder f a -> Encoder f (g a))
  -> GJsonEncoder t f a
  -> GJsonEncoder t f (g a)
wGJEnc1 e (GJEnc ga) =
  GJEnc (e ga)

wGJEnc2
  :: (Encoder f a -> Encoder f b -> Encoder f (g a b))
  -> GJsonEncoder t f a
  -> GJsonEncoder t f b
  -> GJsonEncoder t f (g a b)
wGJEnc2 e (GJEnc ga) (GJEnc gb) =
  GJEnc (e ga gb)

class JsonEncode t a where
  mkEncoder :: Applicative f => GJsonEncoder t f a
  default mkEncoder :: (Applicative f, Generic a, HasDatatypeInfo a, All2 (JsonEncode t) (Code a)) => GJsonEncoder t f a
  mkEncoder = gEncoder defaultOpts

instance JsonEncode t a                   => JsonEncode t (Maybe a)    where mkEncoder = wGJEnc1 E.maybeOrNull mkEncoder
instance (JsonEncode t a, JsonEncode t b) => JsonEncode t (Either a b) where mkEncoder = wGJEnc2 E.either mkEncoder mkEncoder
instance (JsonEncode t a)                 => JsonEncode t [a]          where mkEncoder = wGJEnc1 E.list mkEncoder
instance (JsonEncode t a)                 => JsonEncode t (NonEmpty a) where mkEncoder = wGJEnc1 E.nonempty mkEncoder
instance JsonEncode t Text                                             where mkEncoder = GJEnc E.text
instance JsonEncode t Int                                              where mkEncoder = GJEnc E.int
instance JsonEncode t Scientific                                       where mkEncoder = GJEnc E.scientific
instance JsonEncode t Bool                                             where mkEncoder = GJEnc E.bool

newtype GJsonDecoder t f a = GJDec
  { unGJDec :: Decoder f a
  }

generaliseGJDecoder :: Monad f => GJsonDecoder t Identity a -> GJsonDecoder t f a
generaliseGJDecoder = GJDec . D.generaliseDecoder . unGJDec

wGJDec1
  :: (Decoder f a -> Decoder f (g a))
  -> GJsonDecoder t f a
  -> GJsonDecoder t f (g a)
wGJDec1 e (GJDec ga) =
  GJDec (e ga)

wGJDec2
  :: (Decoder f a -> Decoder f b -> Decoder f (g a b))
  -> GJsonDecoder t f a
  -> GJsonDecoder t f b
  -> GJsonDecoder t f (g a b)
wGJDec2 e (GJDec ga) (GJDec gb) =
  GJDec (e ga gb)

class JsonDecode t a where
  mkDecoder :: Monad f => GJsonDecoder t f a
  default mkDecoder :: (Monad f, Generic a, HasDatatypeInfo a, All2 (JsonDecode t) (Code a)) => GJsonDecoder t f a
  mkDecoder = gDecoder defaultOpts

instance JsonDecode t a                   => JsonDecode t (Maybe a)    where mkDecoder = wGJDec1 D.maybeOrNull mkDecoder
instance (JsonDecode t a, JsonDecode t b) => JsonDecode t (Either a b) where mkDecoder = wGJDec2 D.either mkDecoder mkDecoder
instance (JsonDecode t a)                 => JsonDecode t [a]          where mkDecoder = wGJDec1 D.list mkDecoder
instance (JsonDecode t a)                 => JsonDecode t (NonEmpty a) where mkDecoder = wGJDec1 D.nonempty mkDecoder
instance JsonDecode t Text                                             where mkDecoder = GJDec D.text
instance JsonDecode t Int                                              where mkDecoder = GJDec D.int
instance JsonDecode t Scientific                                       where mkDecoder = GJDec D.scientific
instance JsonDecode t Bool                                             where mkDecoder = GJDec D.bool

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
  -> GJsonEncoder t f a
gEncoder opts = GJEnc . E.encodeA $ \a -> hcollapse $ hcliftA2
  (Proxy :: Proxy (All (JsonEncode t)))
  (gEncoder' pjE opts)
  (jsonInfo opts (Proxy :: Proxy a))
  (unSOP $ from a)
  where
    pjE = Proxy :: Proxy (JsonEncode t)

ge :: (Applicative f, JsonEncode t x) => Proxy (JsonEncode t) -> GJsonEncoder t f x
ge _ = mkEncoder

gEncoder'
  :: forall xs f t.
     ( All (JsonEncode t) xs
     , Applicative f
     )
  => Proxy (JsonEncode t)
  -> Options
  -> JsonInfo xs
  -> NP I xs
  -> K (f Json) xs
gEncoder' _ _ (JsonZero n) Nil           =
  K (E.runEncoder (unGJEnc mkEncoder) (Text.pack n))

gEncoder' p _ (JsonOne tag) (I a :* Nil) =
  tagVal tag $ E.runEncoder (unGJEnc (ge p)) a

gEncoder' p _ (JsonMul tag) cs           =
  tagVal tag . enc . hcollapse $ hcliftA p ik cs
  where
    ik :: JsonEncode t x => I x -> K Json x
    ik = K . runIdentity . E.runEncoder (unGJEnc (ge p)) . unI

    enc = E.runEncoder (E.list E.json)

gEncoder' p opts (JsonRec tag fields) cs    =
  tagVal tag . enc . hcollapse $ hcliftA2 p tup fields cs
  where
    tup :: JsonEncode t x => K String x -> I x -> K (Text, Json) x
    tup f a = K (modFieldName opts (unK f), runIdentity $ E.runEncoder (unGJEnc (ge p)) (unI a))

    enc = E.runEncoder (E.mapToObj E.json id) . Map.fromList

gDecoder
  :: forall f a t.
     ( Generic a
     , HasDatatypeInfo a
     , All2 (JsonDecode t) (Code a)
     , Monad f
     )
  => Options
  -> GJsonDecoder t f a
gDecoder opts = GJDec $ D.Decoder $ \parseFn cursor ->
  to <$> gDecoderConstructor opts (Proxy :: Proxy (All (JsonDecode t))) parseFn cursor (jsonInfo opts (Proxy :: Proxy a))

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
  SOP . unK . inj <$> hsequence (hcliftA pJDec (aux pJDec) val)
  where
    gd :: JsonDecode t x => Proxy (JsonDecode t) -> GJsonDecoder t f x
    gd _ = mkDecoder

    aux :: JsonDecode t x => Proxy (JsonDecode t) -> K Count x -> D.DecodeResult f x
    aux pd (K rnk) = D.moveToRankN rnk cursor >>= D.focus (unGJDec (gd pd))

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
