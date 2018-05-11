{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE OverloadedStrings      #-}
module WaargDraft where

import           Prelude                     (Eq, Int, Show)

import           Control.Applicative         (Alternative, pure, (*>), (<$>),
                                              (<*), (<*>), (<|>))
import           Control.Category            ((.))
import           Control.Lens                (Index, IxValue, Ixed (..), from,
                                              ix, makeClassy, makeClassyPrisms,
                                              to, (%%~), (^..), _2)
import           Control.Monad               (Monad)

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB

import           Data.Bool                   (Bool (..))
import           Data.Char                   (Char)
import           Data.Maybe                  (Maybe (..), maybe)

import           Data.Foldable               (Foldable (..), asum)
import           Data.Function               (const, ($))

import           Data.Functor                (Functor (..))
import           Data.List                   (intersperse)
import           Data.List.NonEmpty          (NonEmpty (..))

import           Data.Monoid                 (mempty)
import           Data.Semigroup              (mconcat, (<>))

import           Data.Traversable            (Traversable (..))

import           Text.Parser.Char            (CharParsing, char, text)
import           Text.Parser.Combinators     (between, many, manyTill, optional,
                                              sepBy, try)

import           Data.Digit                  (Digit, HeXaDeCiMaL)
import           Data.Separated              (Separated (..), separated,
                                              separatedBy)

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

data Comma = Comma
  deriving (Eq, Show)

commaB :: Builder
commaB = BB.charUtf8 ','

parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','

data JsonArr ws a = JsonArr
  { _jsonArrOpeningWS :: ws
  , _jsonArrVals      :: [a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonArr

type instance Index (JsonArr ws a)   = Int
type instance IxValue (JsonArr ws a) = a

instance Ixed (JsonArr ws a) where
  ix i f = jsonArrVals %%~ ix i f
  {-# INLINE ix #-}

data JsonAssoc digit ws a = JsonAssoc
  { _jsonAssocKey           :: JString digit
  , _jsonAssocKeyTrailingWS :: ws
  , _jsonAssocVal           :: a
  , _jsonAssocComma         :: Maybe Comma
  , _jsonAssocTrailingWS    :: ws
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonAssoc

newtype JsonObject digit ws a = JsonObject
  (Separated Comma (JsonAssoc digit ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data JsonObj digit ws a = JsonObj
  { _jsonObjOpeningWS :: ws
  , _jsonObjAssocs    :: [JsonAssoc digit ws a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''JsonObj

type instance Index (JsonObj digit ws a)   = JString digit
type instance IxValue (JsonObj digit ws a) = a

data JTypes digit ws a
  = JNull ws
  | JBool Bool ws
  | JNum JNumber ws
  | JStr (JString digit) ws
  | JArr (JsonArr ws a) ws
  | JObj (JsonObj digit ws a) ws
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassyPrisms ''JTypes

data Json
  = Json WS (JTypes Digit WS Json)
  deriving (Eq, Show)

buildCommaSep
  :: Char
  -> Char
  -> (ws -> Builder)
  -> ((ws -> Builder) -> a -> Builder)
  -> [a]
  -> Builder
buildCommaSep h t sB iB ws =
  let
    commas = intersperse (BB.charUtf8 ',')
    xs = mconcat . commas $ iB sB <$> ws
  in
    BB.charUtf8 h <> xs <> BB.charUtf8 t

parseJsonAssoc
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f (JsonAssoc digit ws Json)
parseJsonAssoc ws = JsonAssoc
  <$> parseJString
  <*> ws
  <* char ':'
  <*> simpleWaargDraft
  <*> optional parseComma
  <*> ws

jsonAssocBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonAssoc digit WS Json
  -> Builder
jsonAssocBuilder sBuilder (JsonAssoc k ktws v mComma tws) =
  jStringBuilder k <>
  sBuilder ktws <>
  BB.charUtf8 ':' <>
  jsonBuilder sBuilder v <>
  maybe mempty (const commaB) mComma <>
  sBuilder tws

jsonsBuilder
  :: (WS -> Builder)
  -> JsonArr WS Json
  -> Builder
jsonsBuilder sBuilder (JsonArr lws jl) =
  let
    commas = intersperse (BB.charUtf8 ',')
    xs = mconcat . commas $ jsonBuilder sBuilder <$> jl
  in
    BB.charUtf8 '[' <> sBuilder lws <> xs <> BB.charUtf8 ']'

jsonObjectBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonObject digit WS Json
  -> Builder
jsonObjectBuilder sBuilder (JsonObject js) =
  BB.charUtf8 '{' <> xs <> BB.charUtf8 '}'
  where
    xs = fold . intersperse commaB $
      js ^.. from separated
      . traverse
      . _2
      . to (jsonAssocBuilder sBuilder)

parseJsonObject
  :: ( Monad f
     , Alternative f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f (JsonObject digit ws Json)
parseJsonObject ws = JsonObject
  <$> (
  char '{' *>
  separatedBy parseComma (parseJsonAssoc ws) <*
  char '}'
  )

parseJObject
  :: ( Monad f
     , CharParsing f
     , HeXaDeCiMaL digit
     )
  => f ws
  -> f (JsonObj digit ws Json)
parseJObject ws = do
  _ <- char '{'
  lws <- ws
  jas <- many $ parseJsonAssoc ws
  _ <- char '}'
  pure $ JsonObj lws jas

jsonObjBuilder
  :: HeXaDeCiMaL digit
  => (WS -> Builder)
  -> JsonObj digit WS Json
  -> Builder
jsonObjBuilder sBuilder (JsonObj lws jL) =
  let
    -- commas = intersperse (BB.charUtf8 ',')
    xs = foldMap (jsonAssocBuilder sBuilder) jL
  in
    BB.charUtf8 '{' <> sBuilder lws <> xs <> BB.charUtf8 '}'

jTypesBuilder
  :: (WS -> Builder)
  -> JTypes Digit WS Json
  -> BB.Builder
jTypesBuilder s (JNull tws)     = BB.stringUtf8 "null"                          <> s tws
jTypesBuilder s (JBool b tws)   = BB.stringUtf8 (if b then "true" else "false") <> s tws
jTypesBuilder s (JNum jn tws)   = jNumberBuilder jn                             <> s tws
jTypesBuilder s (JStr js tws)   = jStringBuilder js                             <> s tws
jTypesBuilder s (JArr js tws)   = jsonsBuilder s js                             <> s tws
jTypesBuilder s (JObj jobj tws) = jsonObjBuilder s jobj                         <> s tws

jsonBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
jsonBuilder ws (Json lws jt) =
  ws lws <> jTypesBuilder ws jt
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

-- |
--
-- >>> testparse (parseJsons parseWhitespace) "[null ]"
-- Right (JsonArr {_jsonArrOpeningWS = WS [], _jsonArrVals = [Json (WS []) (JNull (WS [Space]))]})
--
parseJsons
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JsonArr ws Json)
parseJsons ws = do
  _ <- char '['
  lws <- ws
  vals <- sepBy simpleWaargDraft (char ',')
  _ <- char ']'
  pure $ JsonArr lws vals

parseJArr
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJArr ws =
  JArr <$> parseJsons ws <*> ws

parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseJObj ws =
  JObj <$> parseJObject ws <*> ws

parseWaargDraft
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JTypes Digit ws Json)
parseWaargDraft =
  asum . sequence
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
  Json <$> parseWhitespace <*> parseWaargDraft parseWhitespace
