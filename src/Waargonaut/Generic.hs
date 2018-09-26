{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Lens               (findOf, folded, isn't, _Left)

import           Control.Monad.Except       (lift, throwError)
import           Control.Monad.State        (modify)
import qualified Control.Zipper             as Z

import           Data.Maybe                 (fromMaybe)

import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Data.Map                   as Map

import           Waargonaut                 (Json)

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (..))
import           Waargonaut.Decode.Internal (CursorHistory' (..))

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

class JsonEncode a where
  mkEncoder :: Encoder a
  default mkEncoder :: (Generic a, HasDatatypeInfo a, All2 JsonEncode (Code a)) => Encoder a
  mkEncoder = gEncoder defaultOpts

instance JsonEncode a                 => JsonEncode (Maybe a)    where mkEncoder = E.maybeOrNull mkEncoder
instance (JsonEncode a, JsonEncode b) => JsonEncode (Either a b) where mkEncoder = E.either mkEncoder mkEncoder
instance (JsonEncode a)               => JsonEncode [a]          where mkEncoder = E.list mkEncoder
instance (JsonEncode a)               => JsonEncode (NonEmpty a) where mkEncoder = E.nonempty mkEncoder
instance JsonEncode Text                                         where mkEncoder = E.text
instance JsonEncode Int                                          where mkEncoder = E.int
instance JsonEncode Bool                                         where mkEncoder = E.bool

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

inObj
  :: Encoder a
  -> String
  -> Encoder a
inObj en t =
  E.mapLikeObj (E.atKey (Text.pack t) en)

tagVal
  :: SListI xs
  => Tag
  -> Json
  -> K Json xs
tagVal  NoTag  v = K v
tagVal (Tag t) v = K (E.encodeToJson (inObj E.json t) v)

unTagVal
  :: Monad f
  => Tag
  -> Decoder f a
  -> D.JCursor h Json
  -> D.DecodeResult f a
unTagVal NoTag   d = D.focus d
unTagVal (Tag n) d = D.fromKey (Text.pack n) d

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
      ConstructorNameAsKey -> Tag n

    tag :: NP ConstructorInfo (Code a) -> ConstructorName -> Tag
    tag (_ :* Nil) = const NoTag
    tag _          = Tag

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

gEncoder
  :: forall a.
     ( Generic a
     , HasDatatypeInfo a
     , All2 JsonEncode (Code a)
     )
  => Options
  -> Encoder a
gEncoder opts = E.encodePureA $ \a -> hcollapse $ hcliftA2
  (Proxy :: Proxy (All JsonEncode))
  (gEncoder' opts)
  (jsonInfo opts (Proxy :: Proxy a))
  (unSOP $ from a)

gEncoder'
  :: forall xs.
     All JsonEncode xs
  => Options
  -> JsonInfo xs
  -> NP I xs
  -> K Json xs
gEncoder' _ (JsonZero n) Nil           =
  K (E.encodeToJson mkEncoder (Text.pack n))

gEncoder' _ (JsonOne tag) (I a :* Nil) =
  tagVal tag (E.encodeToJson mkEncoder a)

gEncoder' _ (JsonMul tag) cs           =
  tagVal tag . enc . hcollapse $ hcliftA pJEnc ik cs
  where
    ik :: JsonEncode a => I a -> K Json a
    ik = K . E.encodeToJson mkEncoder . unI

    enc = E.encodeToJson (E.list E.json)

gEncoder' opts (JsonRec tag fields) cs    =
  tagVal tag . enc . hcollapse $ hcliftA2 pJEnc tup fields cs
  where
    tup :: JsonEncode a => K String a -> I a -> K (Text, Json) a
    tup f a = K (modFieldName opts (unK f), E.encodeToJson mkEncoder (unI a))

    enc = E.encodeToJson (E.mapToObj E.json id) . Map.fromList

gDecoder
  :: forall f a.
     ( Generic a
     , HasDatatypeInfo a
     , All2 JsonDecode (Code a)
     , Monad f
     )
  => Options
  -> Decoder f a
gDecoder opts = D.withCursor $ \cursor ->
  to <$> gDecoderConstructor opts cursor (jsonInfo opts (Proxy :: Proxy a))

gDecoderConstructor
  :: forall (xss :: [[*]]) f h.
     ( All2 JsonDecode xss
     , Monad f
     )
  => Options
  -> D.JCursor h Json
  -> NP JsonInfo xss
  -> D.DecodeResult f (SOP I xss)
gDecoderConstructor opts cursor ninfo =
  foldForRight . hcollapse $ hcliftA2 pAllJDec (mkGDecoder opts cursor) ninfo injs
  where
    err = Left (ConversionFailure "Generic Decoder has failed", D.CursorHist (CursorHistory' mempty))

    failure (e,h) = modify (const $ D.unCursorHist h) >> throwError e

    -- Pretty sure there is a better way to manage this, as my intuition about
    -- generic-sop says that I will only have one successful result for any
    -- given type. But I'm not 100% sure that this is actually the case.
    foldForRight :: [D.DecodeResult f (SOP I xss)] -> D.DecodeResult f (SOP I xss)
    foldForRight xs = (lift . sequence $ D.runDecoderResult <$> xs)
      >>= either failure pure
      . fromMaybe err
      . findOf folded (isn't _Left)

    injs :: NP (Injection (NP I) xss) xss
    injs = injections

mkGDecoder
  :: forall (xss :: [[*]]) (xs :: [*]) f h.
     ( All JsonDecode xs
     , SListI xs
     , Monad f
     )
  => Options
  -> D.JCursor h Json
  -> JsonInfo xs
  -> Injection (NP I) xss xs
  -> K (D.DecodeResult f (SOP I xss)) xs
mkGDecoder opts cursor info (Fn inj) = K $ do
  val <- mkGDecoder2 opts cursor info
  SOP . unK . inj <$> hsequence (hcliftA pJDec aux val)
  where
    aux :: JsonDecode a => K Json a -> D.DecodeResult f a
    aux = D.runDecoder mkDecoder . Z.zipper . unK

mkGDecoder2
  :: forall (xs :: [*]) f h.
     ( All JsonDecode xs
     , SListI xs
     , Monad f
    )
  => Options
  -> D.JCursor h Json
  -> JsonInfo xs
  -> D.DecodeResult f (NP (K Json) xs)
mkGDecoder2 _ cursor (JsonZero _) =
  Nil <$ unTagVal NoTag D.json cursor

mkGDecoder2 _ cursor (JsonOne tag) =
  (\j -> K j :* Nil) <$> unTagVal tag D.json cursor

mkGDecoder2 _ cursor (JsonMul tag) = do
  xs <- unTagVal tag (D.list D.json) cursor
  maybe err pure (fromList xs)
  where
    err = throwError (ConversionFailure "Generic List")

mkGDecoder2 opts cursor (JsonRec tag fields) =
  hsequenceK $ hcliftA pJDec (mapKK decodeAtKey) fields
  where
    decodeAtKey k = unTagVal tag (
      D.withCursor $ D.fromKey (modFieldName opts k) D.json
      ) cursor
