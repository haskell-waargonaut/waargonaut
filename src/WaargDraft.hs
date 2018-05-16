{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StandaloneDeriving     #-}
module WaargDraft where

import           Prelude                     (Eq, Show)

import           Control.Applicative         (pure, (*>), (<$>), (<*), (<*>),
                                              (<|>))
import           Control.Category            ((.))
import           Control.Lens                (Index, IxValue, makeClassy,
                                              makeClassyPrisms)
import           Control.Monad               (Monad)

import           Data.Distributive           (distribute)

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB

import           Data.Bool                   (Bool (..))
import           Data.Maybe                  (Maybe (..), maybe)

import           Data.Foldable               (Foldable (..), asum)
import           Data.Function               (($))

import           Data.Functor                (Functor (..))
import           Data.Functor.Identity       (Identity (..))

import           Data.Monoid                 (mempty)
import           Data.Semigroup              ((<>))

import           Data.Traversable            (Traversable (..))

import           Text.Parser.Char            (CharParsing, char, text)

import           Data.Digit                  (Digit, HeXaDeCiMaL)

import           Waargonaut.Types.CommaSep   (Comma (Comma),
                                              commaTrailingBuilder,
                                              parseCommaSepOpTrailing,
                                              parseCommaTrailingIdentity,
                                              parseCommaTrailingMaybe)
import           Waargonaut.Types.JNumber
import           Waargonaut.Types.JString
import           Waargonaut.Types.Whitespace

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParseError)
-- >>> import Data.Digit (Digit)
----

data JsonArrElem f ws a = JsonArrElem
  { _jsonArrElemVal      :: a
  , _jsonArrElemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq ws, Eq a) => Eq (JsonArrElem Identity ws a)
deriving instance (Eq ws, Eq a) => Eq (JsonArrElem Maybe ws a)

deriving instance (Show ws, Show a) => Show (JsonArrElem Identity ws a)
deriving instance (Show ws, Show a) => Show (JsonArrElem Maybe ws a)

data JArray ws a
  = JArray ws (Maybe (JsonArr ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data JsonArr ws a = JsonArr
  { _jsonArrVals    :: [JsonArrElem Identity ws a]
  , _jsonArrValLast :: JsonArrElem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonArr

singletonJsonArr
  :: a
  -> Maybe (Comma, ws)
  -> JsonArr ws a
singletonJsonArr a tws =
  JsonArr [] (JsonArrElem a tws)

consJsonArr
  :: a
  -> ws
  -> JsonArr ws a
  -> JsonArr ws a
consJsonArr a elemWS (JsonArr [] lst) =
  JsonArr [JsonArrElem a (Identity (Comma, elemWS))] lst
consJsonArr a elemWS (JsonArr xs lst) =
  JsonArr (JsonArrElem a (Identity (Comma, elemWS)) : xs) lst

data JsonAssoc digit ws a = JsonAssoc
  { _jsonAssocKey             :: JString digit
  , _jsonAssocKeyTrailingWS   :: ws
  , _jsonAssocValPreceedingWS :: ws
  , _jsonAssocVal             :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonAssoc

data JsonObjElem f digit ws a = JsonObjElem
  { _jsonObjElemVal      :: JsonAssoc digit ws a
  , _jsonObjElemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Show digit, Show ws, Show a) => Show (JsonObjElem Identity digit ws a)
deriving instance (Show digit, Show ws, Show a) => Show (JsonObjElem Maybe digit ws a)

deriving instance (Eq digit, Eq ws, Eq a) => Eq (JsonObjElem Identity digit ws a)
deriving instance (Eq digit, Eq ws, Eq a) => Eq (JsonObjElem Maybe digit ws a)

data JsonObj digit ws a = JsonObj
  { _jsonObjAssocs   :: [JsonObjElem Identity digit ws a]
  , _jsonObjLastElem :: JsonObjElem Maybe digit ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonObj

data JObject digit ws a =
  JObject ws (Maybe (JsonObj digit ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Index (JsonObj digit ws a)   = JString digit
type instance IxValue (JsonObj digit ws a) = a

data JTypes digit ws a
  = JNull ws
  | JBool Bool ws
  | JNum JNumber ws
  | JStr (JString digit) ws
  | JArr (JArray ws a) ws
  | JObj (JObject digit ws a) ws
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassyPrisms ''JTypes

newtype Json
  = Json (JTypes Digit WS Json)
  deriving (Eq, Show)

parseJsonAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JsonAssoc digit ws a)
parseJsonAssoc ws a = JsonAssoc
  <$> parseJString <*> ws <* char ':' <*> ws <*> a

parseJsonAssocs
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JsonObj digit ws a)
parseJsonAssocs ws a = do
  let
    idElem e = JsonObjElem e . Identity

  hd <- parseJsonAssoc ws a
  sep <- parseCommaTrailingMaybe ws
  maybe
    (pure $ JsonObj [] (JsonObjElem hd sep))
    (parseCommaSepOpTrailing idElem JsonObjElem JsonObj ws (parseJsonAssoc ws a) [] . (hd,))
    sep

jsonAssocBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonAssoc digit WS Json
  -> Builder
jsonAssocBuilder sBuilder (JsonAssoc k ktws vpws v) =
  jStringBuilder k <>
  sBuilder ktws <>
  BB.charUtf8 ':' <>
  sBuilder vpws <>
  jsonBuilder sBuilder v

jsonObjElemBuilder
  :: ( Functor f
     , Foldable f
     , HeXaDeCiMaL digit
     )
  => (WS -> Builder)
  -> JsonObjElem f digit WS Json
  -> Builder
jsonObjElemBuilder sBuilder (JsonObjElem e tws) =
  jsonAssocBuilder sBuilder e <> commaTrailingBuilder sBuilder tws

jsonsBuilder
  :: (WS -> Builder)
  -> JsonArr WS Json
  -> Builder
jsonsBuilder sBuilder (JsonArr jElems jElemLast) =
  let
    jb :: ( Functor f
          , Foldable f
          )
       => JsonArrElem f WS Json
       -> Builder
    jb (JsonArrElem e mTrailing) =
      jsonBuilder sBuilder e <> commaTrailingBuilder sBuilder mTrailing

    elemsB = foldMap jb jElems <> jb jElemLast
  in
    elemsB

jArrayBuilder
  :: (WS -> Builder)
  -> JArray WS Json
  -> Builder
jArrayBuilder ws (JArray lws jsons) =
  BB.charUtf8 '[' <> ws lws <> maybe mempty (jsonsBuilder ws) jsons <> BB.charUtf8 ']'

parseJObject
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f a
  -> f (JObject digit ws a)
parseJObject ws a =
  char '{' *> (
    JObject <$> ws <*> asum
      [ Nothing <$ char '}'
      , Just <$> parseJsonAssocs ws a <* char '}'
      ]
  )

jsonObjBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonObj digit WS Json
  -> Builder
jsonObjBuilder sBuilder (JsonObj elems lastelem) =
  foldMap (jsonObjElemBuilder sBuilder) elems <> jsonObjElemBuilder sBuilder lastelem

jObjectBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JObject digit WS Json
  -> Builder
jObjectBuilder sBuilder (JObject lws jobjs) =
  BB.charUtf8 '{' <> sBuilder lws <> maybe mempty (jsonObjBuilder sBuilder) jobjs <> BB.charUtf8 '}'

jTypesBuilder
  :: (WS -> Builder)
  -> JTypes Digit WS Json
  -> BB.Builder
jTypesBuilder s (JNull tws)     = BB.stringUtf8 "null"                          <> s tws
jTypesBuilder s (JBool b tws)   = BB.stringUtf8 (if b then "true" else "false") <> s tws
jTypesBuilder s (JNum jn tws)   = jNumberBuilder jn                             <> s tws
jTypesBuilder s (JStr js tws)   = jStringBuilder js                             <> s tws
jTypesBuilder s (JArr js tws)   = jArrayBuilder s js                            <> s tws
jTypesBuilder s (JObj jobj tws) = jObjectBuilder s jobj                         <> s tws

jsonBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
jsonBuilder ws (Json jt) =
  jTypesBuilder ws jt
-- |
--
-- >>> testparse (parseJNull (return ())) "null"
-- Right (JNull ())
--
-- >>> testparsetheneof (parseJNull (return ())) "null"
-- Right (JNull ())
--
-- >>> testparsethennoteof (parseJNull (return ())) "nullx"
-- Right (JNull ())
--
parseJNull
  :: ( CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJNull ws = JNull
  <$ text "null"
  <*> ws

-- |
--
-- >>> testparse (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparse (parseJBool (return ())) "false"
-- Right (JBool False ())
---
-- >>> testparsetheneof (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparsetheneof (parseJBool (return ())) "false"
-- Right (JBool False ())
---
-- >>> testparsethennoteof (parseJBool (return ())) "truex"
-- Right (JBool True ())
--
-- >>> testparsethennoteof (parseJBool (return ())) "falsex"
-- Right (JBool False ())
--
parseJBool
  :: ( CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJBool ws =
  let
    b q t = JBool q <$ text t
  in
    (b False "false" <|> b True "true") <*> ws

parseJNum
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws a)
parseJNum ws =
  JNum <$> parseJNumber <*> ws

-- |
--
-- >>> testparse (parseJStr (return ())) "\"\""
-- Right (JStr (JString []) ())
--
-- >>> testparse (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >> testparse (parseJStr (return ())) "\"a\\rbc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either ParseError (JTypes Digit () a)
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"\""
-- Right (JStr (JString []) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\\u"
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\t"
-- Right (JStr (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJStr
  :: CharParsing f
  => f ws
  -> f (JTypes Digit ws a)
parseJStr ws =
  JStr <$> parseJString <*> ws

parseJsonArrElemMaybe
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JsonArrElem Maybe ws a)
parseJsonArrElemMaybe ws a = JsonArrElem
  <$> a <*> parseCommaTrailingMaybe ws

parseJsonArrElemIdentity
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (JsonArrElem Identity ws a)
parseJsonArrElemIdentity ws a = JsonArrElem
  <$> a <*> parseCommaTrailingIdentity ws

parseJsons
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JsonArr ws Json)
parseJsons ws = do
  let
    idCons a = JsonArrElem a . Identity
    mayCons a = JsonArrElem a

  hd <- simpleWaargDraft
  sep <- parseCommaTrailingMaybe ws
  maybe
    (pure $ singletonJsonArr hd sep)
    (parseCommaSepOpTrailing idCons mayCons JsonArr ws simpleWaargDraft [] . (hd,))
    sep

-- |
--
-- >>> testparse (parseJArray parseWhitespace) "[null ]"
-- Right (JArray (WS []) (Just (JsonArr {_jsonArrVals = [], _jsonArrValLast = JsonArrElem {_jsonArrElemVal = Json (JNull (WS [Space])), _jsonArrElemTrailing = Nothing}})))
--
-- >>> testparse (parseJArray parseWhitespace) "[null,]"
-- Right (JArray (WS []) (Just (JsonArr {_jsonArrVals = [], _jsonArrValLast = JsonArrElem {_jsonArrElemVal = Json (JNull (WS [])), _jsonArrElemTrailing = Just (Comma,WS [])}})))
--
parseJArray
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JArray ws Json)
parseJArray ws = char '[' *>
  (
    JArray <$> ws <*> asum
      [ Nothing <$ char ']'
      , Just <$> parseJsons ws <* char ']'
      ]
  )

parseJArr
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJArr ws =
  JArr <$> parseJArray ws <*> ws

parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJObj ws =
  JObj <$> parseJObject ws simpleWaargDraft <*> ws

parseWaargDraft
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseWaargDraft =
  asum . distribute
    [ parseJNull
    , parseJBool
    , parseJNum
    , parseJStr
    , parseJArr
    , parseJObj
    ]

simpleWaargDraft
  :: ( Monad f
     , CharParsing f
     )
  => f Json
simpleWaargDraft =
  Json <$> parseWaargDraft parseWhitespace
