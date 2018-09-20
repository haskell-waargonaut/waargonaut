{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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

import           Control.Monad.Except       (lift, throwError)

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

instance JsonDecode a                 => JsonDecode (Maybe a)    where mkDecoder = D.maybe mkDecoder
instance (JsonDecode a, JsonDecode b) => JsonDecode (Either a b) where mkDecoder = D.either mkDecoder mkDecoder
instance (JsonDecode a)               => JsonDecode [a]          where mkDecoder = D.list mkDecoder
instance (JsonDecode a)               => JsonDecode (NonEmpty a) where mkDecoder = D.nonempty mkDecoder
instance JsonDecode Text                                         where mkDecoder = D.text
instance JsonDecode Int                                          where mkDecoder = D.int
instance JsonDecode Bool                                         where mkDecoder = D.boolean

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

unTagVal :: Monad f => Tag -> Decoder f a -> D.JCursor h Json -> D.DecodeResult f a
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

pJDec :: Proxy JsonDecode
pJDec = Proxy

pAllJDec :: Proxy (All JsonDecode)
pAllJDec = Proxy

gEncoder' :: forall xs. All JsonEncode xs => Options -> JsonInfo xs -> NP I xs -> K Json xs
gEncoder' _ (JsonZero n) Nil           = K (E.encodeToJson mkEncoder (Text.pack n))
gEncoder' _ (JsonOne tag) (I a :* Nil) = tagVal tag (E.encodeToJson mkEncoder a)

gEncoder' _ (JsonMul tag) cs           =
  tagVal tag (E.encodeToJson (E.list E.json) (hcollapse $ hcliftA pJEnc (K . E.encodeToJson mkEncoder . unI) cs))

gEncoder' opts (JsonRec tag fields) cs    = tagVal tag
  . (E.encodeToJson (E.mapToObj E.json id) . Map.fromList) . hcollapse
  $ hcliftA2 pJEnc (\f a -> K (Text.pack (_optionsFieldName opts $ unK f), E.encodeToJson mkEncoder (unI a))) fields cs

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
  to <$> gDecoderConstructor opts cursor (jsonInfo opts (Proxy @a))

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

    foldForRight :: [D.DecodeResult f (SOP I xss)] -> D.DecodeResult f (SOP I xss)
    foldForRight xs = do
      ys' <- lift . sequence $ D.runDecoderResult <$> xs
      either (throwError . fst) pure (foldr keepFirstRight err ys')

    keepFirstRight :: Either e a -> Either e a -> Either e a
    keepFirstRight _           l@(Left _)  = l
    keepFirstRight r@(Right _) _           = r
    keepFirstRight _           r@(Right _) = r

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
  prod <- hsequence $ hcliftA pJDec aux val
  pure $ SOP . unK . inj $ prod
  where
    aux :: JsonDecode a => K Json a -> D.DecodeResult f a
    aux (K _) = D.runDecoder mkDecoder cursor

mkGDecoder2
  :: forall (xs :: [*]) f h.
     ( All JsonDecode xs
     , SListI xs
     , Monad f
    )
  => Options
  -> D.JCursor h Json
  -> JsonInfo xs
  -- -> NP (K (f (Either (DecodeError, D.CursorHistory) Json))) xs
  -- -> K (f (Either (DecodeError, D.CursorHistory) (NP (K Json) xs))) xs
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

-- mkGDecoder2 opts cursor (JsonMul tag) =
--   let
--     vols :: D.DecodeResult f [Json]
--     vols = unTagVal tag (D.list D.json) cursor

--     vals :: D.DecodeResult f (NP (K Json) xs)
--     vals = do
--       xs <- vols
--       case fromList xs of
--         Just v -> pure v
--         Nothing -> throwError (ConversionFailure "Generic List")
--   in
--     case fromList vols of
--       Just v  -> v
--       Nothing ->
--         let
--           err = Left (ConversionFailure "Generic List", D.CursorHist (CursorHistory' mempty))
--         in
--           hpure $ K (return err)

mkGDecoder2 opts cursor (JsonRec tag fields) =
  hsequenceK $ hcliftA pJDec (mapKK (kdec cursor)) fields
  where
    modFieldName = Text.pack . _optionsFieldName opts

    kdec :: D.JCursor h Json -> String -> D.DecodeResult f Json
    kdec c k = unTagVal tag (D.withCursor $ \c' -> D.fromKey (modFieldName k) D.json c') c

-- mkGDecoder'
--   :: forall (xss :: [[*]]) (xs :: [*]) f h a.
--      ( All JsonDecode xs
--      , SListI xs
--      , Monad f
--     )
--   => Options
--   -> D.JCursor h Json
--   -> JsonInfo xs
--   -> D.DecodeResult f (NP (K a) xs)
-- mkGDecoder' opts cursor (JsonZero _)  = Nil                <$  unTagVal NoTag mkDecoder cursor
-- mkGDecoder' opts cursor (JsonOne tag) = (\v -> K v :* Nil) <$> unTagVal tag mkDecoder cursor
-- mkGDecoder' opts cursor (JsonMul tag) = do
--   xs <- unTagVal tag (D.list mkDecoder) cursor
--   case fromList xs of
--     Just v  -> pure v
--     Nothing -> throwError (ConversionFailure "Attempted list decode")

-- mkGDecoder' opts cursor (JsonRec tag fields) =
--   hsequenceK $ hcliftA (Proxy @JsonDecode) (mapKK (\k -> D.fromKey (Text.pack k) mkDecoder cursor)) fields
