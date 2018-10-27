{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Waargonaut.Generic
  ( JsonEncode (..)
  , JsonDecode (..)
  , NewtypeName (..)
  , Options (..)
  , defaultOpts
  , gEncoder
  , gDecoder
  ) where

import           Generics.SOP

import           Control.Lens                  (Rewrapped, Wrapped (..), findOf,
                                                folded, isn't, _Left)

import           Control.Monad                 ((>=>))
import           Control.Monad.Except          (lift, throwError)
import           Control.Monad.State           (modify)

import           Data.Functor.Contravariant    (Contravariant (..), (>$<))
import           Data.Functor.Identity         (Identity)

import           Control.Monad.Morph           (MFunctor (..), generalize)

import           Data.Maybe                    (fromMaybe)

import           Data.Functor.Identity         (runIdentity)

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
  { unGJEnc :: Applicative f => Encoder f a
  }

instance (GJsonEncoder t f a) ~ Encoder f a => Rewrapped (GJsonEncoder t f a) (Encoder f a)

instance Wrapped (GJsonEncoder t f a) where
  type Unwrapped (GJsonEncoder t f a) = Encoder f a
  _Wrapped' = iso unGJEnc GJsonEncoder

instance Contravariant (GJsonEncoder t f) where
  contramap f (GJEnc e) = GJEnc (contramap f e)

instance MFunctor (GJsonEncoder t) where
  hoist nat (GJEnc e) = GJEnc (hoist nat e)

generaliseGJEncoder :: Monad f => GJsonEncoder t Identity a -> GJsonEncoder t f a
generaliseGJEncoder (GJEnc e) = GJEnc (E.generaliseEncoder' e)

data GWaarg

class JsonEncode t a where
  mkEncoder :: Applicative f => GJsonEncoder t f a
  default mkEncoder :: (Applicative f, Generic a, HasDatatypeInfo a, All2 (JsonEncode t) (Code a)) => GJsonEncoder t f a
  mkEncoder = gEncoder defaultOpts

instance JsonEncode t a                   => JsonEncode t (Maybe a)    where mkEncoder = E.maybeOrNull mkEncoder
instance (JsonEncode t a, JsonEncode t b) => JsonEncode t (Either a b) where mkEncoder = E.either mkEncoder mkEncoder
instance (JsonEncode t a)                 => JsonEncode t [a]          where mkEncoder = E.list mkEncoder
instance (JsonEncode t a)                 => JsonEncode t (NonEmpty a) where mkEncoder = E.nonempty mkEncoder
instance JsonEncode t Text                                             where mkEncoder = E.text
instance JsonEncode t Int                                              where mkEncoder = E.int
instance JsonEncode t Scientific                                       where mkEncoder = E.scientific
instance JsonEncode t Bool                                             where mkEncoder = E.bool

class JsonDecode a where
  mkDecoder :: Monad f => Decoder f a
  default mkDecoder :: (Monad f, Generic a, HasDatatypeInfo a, All2 JsonDecode (Code a)) => Decoder f a
  mkDecoder = gDecoder defaultOpts

instance JsonDecode a                 => JsonDecode (Maybe a)    where mkDecoder = D.maybeOrNull mkDecoder
instance (JsonDecode a, JsonDecode b) => JsonDecode (Either a b) where mkDecoder = D.either mkDecoder mkDecoder
instance (JsonDecode a)               => JsonDecode [a]          where mkDecoder = D.list mkDecoder
instance (JsonDecode a)               => JsonDecode (NonEmpty a) where mkDecoder = D.nonempty mkDecoder
instance JsonDecode Text                                         where mkDecoder = D.text
instance JsonDecode Int                                          where mkDecoder = D.int
instance JsonDecode Scientific                                   where mkDecoder = D.scientific
instance JsonDecode Bool                                         where mkDecoder = D.bool

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

pJEnc :: Proxy JsonEncode
pJEnc = Proxy

pJDec :: Proxy JsonDecode
pJDec = Proxy

pAllJDec :: Proxy (All JsonDecode)
pAllJDec = Proxy

modFieldName
  :: Options
  -> String
  -> Text
modFieldName opts =
 Text.pack . _optionsFieldName opts

inObj :: Encoder' a -> String -> Encoder' a
inObj en t = E.mapLikeObj' (E.atKey' (Text.pack t) en)

tagVal
  :: ( Applicative f
     , SListI xs
     )
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
  (gEncoder' opts)
  (jsonInfo opts (Proxy :: Proxy a))
  (unSOP $ from a)

gEncoder'
  :: forall xs f t.
     ( All (JsonEncode t) xs
     , Applicative f
     )
  => Options
  -> JsonInfo xs
  -> NP I xs
  -> K (f Json) xs
gEncoder' _ (JsonZero n) Nil           =
  K (E.runEncoder (unGJEnc mkEncoder) (Text.pack n))

gEncoder' _ (JsonOne tag) (I a :* Nil) =
  tagVal tag (E.runEncoder (unGJEnc mkEncoder) a)

gEncoder' _ (JsonMul tag) cs           =
  tagVal tag . enc . hcollapse $ hcliftA pJEnc ik cs
  where
    ik :: JsonEncode t a => I a -> K Json a
    ik = K . runIdentity . E.runEncoder (unGJEnc mkEncoder) . unI

    enc = E.runEncoder (E.list E.json)

gEncoder' opts (JsonRec tag fields) cs    =
  tagVal tag . enc . hcollapse $ hcliftA2 pJEnc tup fields cs
  where
    tup :: JsonEncode t a => K String a -> I a -> K (Text, Json) a
    tup f a = K (modFieldName opts (unK f), runIdentity $ E.runEncoder (unGJEnc mkEncoder) (unI a))

    enc = E.runEncoder (E.mapToObj E.json id) . Map.fromList

gDecoder
  :: forall f a.
     ( Generic a
     , HasDatatypeInfo a
     , All2 JsonDecode (Code a)
     , Monad f
     )
  => Options
  -> Decoder f a
gDecoder opts = D.Decoder $ \parseFn cursor ->
  to <$> gDecoderConstructor opts parseFn cursor (jsonInfo opts (Proxy :: Proxy a))

gDecoderConstructor
  :: forall (xss :: [[*]]) f.
     ( All2 JsonDecode xss
     , Monad f
     )
  => Options
  -> D.ParseFn
  -> D.JCurs
  -> NP JsonInfo xss
  -> DecodeResultT Count DecodeError f (SOP I xss)
gDecoderConstructor opts parseFn cursor ninfo =
  foldForRight . hcollapse $ hcliftA2 pAllJDec (mkGDecoder opts parseFn cursor) ninfo injs
  where
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
  :: forall (xss :: [[*]]) (xs :: [*]) f.
     ( All JsonDecode xs
     , SListI xs
     , Monad f
     )
  => Options
  -> D.ParseFn
  -> D.JCurs
  -> JsonInfo xs
  -> Injection (NP I) xss xs
  -> K (D.DecodeResult f (SOP I xss)) xs
mkGDecoder opts _parseFn cursor info (Fn inj) = K $ do
  val <- mkGDecoder2 opts cursor info
  SOP . unK . inj <$> hsequence (hcliftA pJDec aux val)
  where
    aux :: JsonDecode a => K Count a -> D.DecodeResult f a
    aux (K rnk) = D.moveToRankN rnk cursor >>= D.focus mkDecoder

mkGDecoder2
  :: forall (xs :: [*]) f.
     ( All JsonDecode xs
     , SListI xs
     , Monad f
    )
  => Options
  -> D.JCurs
  -> JsonInfo xs
  -> D.DecodeResult f (NP (K Count) xs)
mkGDecoder2 _ cursor (JsonZero _) =
  Nil <$ unTagVal NoTag D.rank cursor

mkGDecoder2 _ cursor (JsonOne tag) =
  (\j -> K j :* Nil) <$> unTagVal tag D.rank cursor

mkGDecoder2 _ cursor (JsonMul tag) = do
  xs <- unTagVal tag (D.list D.rank) cursor
  maybe err pure (fromList xs)
  where
    err = throwError (ConversionFailure "Generic List Decode Failed")

mkGDecoder2 opts cursor (JsonRec tag fields) = do
  c' <- D.down cursor
  hsequenceK $ hcliftA pJDec (mapKK (decodeAtKey c')) fields
  where
    decodeAtKey c k = unTagVal tag (
      D.withCursor $ D.fromKey (modFieldName opts k) D.rank
      ) c
