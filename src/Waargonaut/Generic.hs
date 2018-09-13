{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Waargonaut.Generic
  ( JsonEncode (..)
  , NewtypeName (..)
  , Options (..)
  , defaultOpts
  , gEncoder
  ) where

import           Generics.SOP

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)
import qualified Data.Text          as Text

import qualified Data.Map           as Map

import           Waargonaut         (Json)

import           Waargonaut.Encode  (Encoder)
import qualified Waargonaut.Encode  as E

data NewtypeName
  = Unwrap
  | ConstructorNameAsKey

data Options = Options
  { _optionsFieldName :: String -> String
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

type JTag = String

data Tag = NoTag | Tag JTag

data JsonInfo :: [*] -> * where
  JsonZero :: ConstructorName -> JsonInfo '[]
  JsonOne  :: Tag -> JsonInfo '[a]
  JsonMul  :: SListI xs => Tag -> JsonInfo xs
  JsonRec  :: SListI xs => Tag -> NP (K String) xs -> JsonInfo xs

inObj :: Encoder a -> String -> Encoder a
inObj en t = E.mapLikeObj (E.atKey en (Text.pack t))

tagVal :: SListI xs => Tag -> Json -> K Json xs
tagVal  NoTag  v = K v
tagVal (Tag t) v = K (E.encodeToJson (inObj E.json t) v)

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
    fname (FieldInfo name) = K (_optionsFieldName opts name)

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

gEncoder
  :: forall a.
     ( Generic a
     , HasDatatypeInfo a
     , All2 JsonEncode (Code a)
     )
  => Options
  -> Encoder a
gEncoder opts = E.encodePureA $ \a -> hcollapse $ hcliftA2
  (Proxy @(All JsonEncode))
  (gEncoder' opts)
  (jsonInfo opts (Proxy @a))
  (unSOP $ from a)

pJEnc :: Proxy JsonEncode
pJEnc = Proxy

gEncoder' :: forall xs. All JsonEncode xs => Options -> JsonInfo xs -> NP I xs -> K Json xs
gEncoder' _ (JsonZero n) Nil           = K (E.encodeToJson E.text (Text.pack n))
gEncoder' _ (JsonOne tag) (I a :* Nil) = tagVal tag (E.encodeToJson mkEncoder a)

gEncoder' _ (JsonMul tag) cs           =
  tagVal tag (E.encodeToJson (E.list E.json) (hcollapse $ hcliftA pJEnc (K . E.encodeToJson mkEncoder . unI) cs))

gEncoder' opts (JsonRec tag fields) cs    = tagVal tag
  . (E.encodeToJson (E.mapToObj E.json id) . Map.fromList)
  . hcollapse
  $ hcliftA2 pJEnc (\f a -> K (Text.pack (_optionsFieldName opts $ unK f), E.encodeToJson mkEncoder (unI a))) fields cs
