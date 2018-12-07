{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Top level types and functions for Waargonaut 'Json' types.
module Waargonaut.Types.Json
  (
    -- * Inner JSON types
    JType (..)
  , AsJType (..)

    -- * Top level JSON type
  , Json (..)

    -- * Parser / Builder
  , waargonautBuilder
  , parseWaargonaut

  -- * Traversals
  , jsonTraversal
  , jsonWSTraversal
  , jtypeTraversal
  , jtypeWSTraversal

  , prettyObj
  , prettyArr

  -- * Optics
  , oat
  , oix
  , aix
  ) where


import           Prelude                     (Eq, Int, Show, (+), (-))

import           Control.Applicative         (pure, (<$>), (<*>), (<|>))
import           Control.Category            (id, (.))
import           Control.Lens                (Prism', Rewrapped, Traversal,
                                              Traversal', Wrapped (..), at, iso,
                                              ix, over, prism, traverseOf, (%~),
                                              (.~), _1, _2, _Just, _Wrapped)

import qualified Control.Lens                as L
import qualified Control.Lens.Plated         as P

import           Control.Monad               (Monad)

import           Data.Bifoldable             (Bifoldable (bifoldMap))
import           Data.Bifunctor              (Bifunctor (bimap))
import           Data.Bitraversable          (Bitraversable (bitraverse))
import           Data.Bool                   (Bool (..))
import           Data.Distributive           (distribute)
import           Data.Either                 (Either (..))
import           Data.Foldable               (Foldable (..), asum, length)
import           Data.Function               (flip, ($))
import           Data.Functor                (Functor (..))
import           Data.Monoid                 (Monoid (..))
import           Data.Semigroup              (Semigroup, (<>))
import           Data.Traversable            (Traversable (..))
import           Data.Tuple                  (uncurry)

import qualified Data.Vector                 as V

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB
import           Data.Maybe                  (Maybe (..), maybe)
import           Data.Text                   (Text)

import           Text.Parser.Char            (CharParsing, text)
import           Waargonaut.Types.CommaSep   (Elems)
import qualified Waargonaut.Types.CommaSep   as CS
import           Waargonaut.Types.JArray     (JArray (..), jArrayBuilder,
                                              parseJArray)
import           Waargonaut.Types.JNumber    (JNumber, jNumberBuilder,
                                              parseJNumber)
import           Waargonaut.Types.JObject    (HasJAssoc (..), JAssoc,
                                              JObject (..), jObjectBuilder,
                                              parseJObject, _MapLikeObj)
import           Waargonaut.Types.JString    (JString, jStringBuilder,
                                              parseJString)
import           Waargonaut.Types.Whitespace (WS (..), Whitespace (..),
                                              parseWhitespace)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Control.Lens
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Data.Function (($))
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Data.Digit (HeXDigit)
-- >>> import qualified Waargonaut.Encode as E
-- >>> let intList = E.runPureEncoder (E.list E.int) [1,2,3]
-- >>> data Foo = Foo { fooA :: Int, fooB :: Text } deriving Show
-- >>> let encodeFoo = E.mapLikeObj $ \(Foo i t) -> E.atKey' "a" E.int i . E.atKey' "b" E.text t
-- >>> let obj = E.runPureEncoder encodeFoo (Foo 33 "Fred")
----

-- | Individual JSON Types and their trailing whitespace.
data JType ws a
  = JNull ws
  | JBool Bool ws
  | JNum JNumber ws
  | JStr JString ws
  | JArr (JArray ws a) ws
  | JObj (JObject ws a) ws
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor JType where
  bimap f g jt = case jt of
    JNull ws   -> JNull (f ws)
    JBool b ws -> JBool b (f ws)
    JNum n ws  -> JNum n (f ws)
    JStr s ws  -> JStr s (f ws)
    JArr a ws  -> JArr (bimap f g a) (f ws)
    JObj o ws  -> JObj (bimap f g o) (f ws)

instance Bifoldable JType where
  bifoldMap f g jt = case jt of
    JNull ws   -> f ws
    JBool _ ws -> f ws
    JNum _ ws  -> f ws
    JStr _ ws  -> f ws
    JArr a ws  -> bifoldMap f g a `mappend` f ws
    JObj o ws  -> bifoldMap f g o `mappend` f ws

instance Bitraversable JType where
  bitraverse f g jt = case jt of
    JNull ws   -> JNull <$> f ws
    JBool b ws -> JBool b <$> f ws
    JNum n ws  -> JNum n <$> f ws
    JStr s ws  -> JStr s <$> f ws
    JArr a ws  -> JArr <$> bitraverse f g a <*> f ws
    JObj o ws  -> JObj <$> bitraverse f g o <*> f ws

-- | Typeclass for things that can represent a 'JType'
class AsJType r ws a | r -> ws a where
  _JType :: Prism' r (JType ws a)
  _JNull  :: Prism' r ws
  _JBool  :: Prism' r (Bool, ws)
  _JNum   :: Prism' r (JNumber, ws)
  _JStr   :: Prism' r (JString, ws)
  _JArr   :: Prism' r (JArray ws a, ws)
  _JObj   :: Prism' r (JObject ws a, ws)

  _JNull = _JType . _JNull
  _JBool = _JType . _JBool
  _JNum  = _JType . _JNum
  _JStr  = _JType . _JStr
  _JArr  = _JType . _JArr
  _JObj  = _JType . _JObj

instance AsJType (JType ws a) ws a where
 _JType = id
 _JNull = prism JNull
       (\ x -> case x of
               JNull ws -> Right ws
               _        -> Left x
       )
 _JBool = prism (uncurry JBool)
       (\ x -> case x of
               JBool j ws -> Right (j, ws)
               _          -> Left x
       )
 _JNum = prism (uncurry JNum)
       (\ x -> case x of
               JNum j ws -> Right (j, ws)
               _         -> Left x
       )
 _JStr = prism (uncurry JStr)
       (\ x -> case x of
               JStr j ws -> Right (j, ws)
               _         -> Left x
       )
 _JArr = prism (uncurry JArr)
       (\ x -> case x of
               JArr j ws -> Right (j, ws)
               _         -> Left x
       )
 _JObj = prism (uncurry JObj)
       (\ x -> case x of
               JObj j ws -> Right (j, ws)
               _         -> Left x
       )

-- | Top level Json type, we specialise the whitespace to 'WS' and the 'digit'
-- type to 'Digit'. Also defining that our structures can recursively only contain
-- 'Json' types.
newtype Json
  = Json (JType WS Json)
  deriving (Eq, Show)

instance Json ~ t => Rewrapped Json t
instance Wrapped Json where
  type Unwrapped Json = JType WS Json
  _Wrapped' = iso (\(Json x) -> x) Json

-- | 'Json' is comprised of the different 'JType' types.
instance AsJType Json WS Json where
  _JType = _Wrapped . _JType

-- | Ignoring whitespace, traverse a 'Json' structure.
jsonTraversal :: Traversal' Json Json
jsonTraversal = traverseOf (_Wrapped . jtypeTraversal)

-- | Traverse the trailing whitespace of this 'Json' structure.
jsonWSTraversal :: Traversal Json Json WS WS
jsonWSTraversal = traverseOf (_Wrapped . jtypeWSTraversal)

-- | Traverse all of the whitespace of this 'Json' structure and every element
-- in the tree.
jtypeWSTraversal :: Traversal (JType ws a) (JType ws' a) ws ws'
jtypeWSTraversal = flip bitraverse pure

-- | Traverse the possible values of a 'JType', skipping whitespace.
jtypeTraversal :: Traversal (JType ws a) (JType ws a') a a'
jtypeTraversal = bitraverse pure

-- | Using the provided whitespace builder, create a builder for a given 'JType'.
jTypesBuilder
  :: (WS -> Builder)
  -> JType WS Json
  -> BB.Builder
jTypesBuilder s (JNull tws)     = BB.stringUtf8 "null"                          <> s tws
jTypesBuilder s (JBool b tws)   = BB.stringUtf8 (if b then "true" else "false") <> s tws
jTypesBuilder s (JNum jn tws)   = jNumberBuilder jn                             <> s tws
jTypesBuilder s (JStr js tws)   = jStringBuilder js                             <> s tws
jTypesBuilder s (JArr js tws)   = jArrayBuilder s waargonautBuilder js          <> s tws
jTypesBuilder s (JObj jobj tws) = jObjectBuilder s waargonautBuilder jobj       <> s tws

immediateTrailingWS :: Traversal' Json WS
immediateTrailingWS f = traverseOf _Wrapped $ \jt -> case jt of
  JNull ws   -> JNull   <$> f ws
  JBool b ws -> JBool b <$> f ws
  JNum n ws  -> JNum n  <$> f ws
  JStr s ws  -> JStr s  <$> f ws
  JArr a ws  -> JArr a  <$> f ws
  JObj o ws  -> JObj o  <$> f ws

objelems :: AsJType r ws a => Traversal' r (Elems ws (JAssoc ws a))
objelems = _JObj . _1 . _Wrapped . CS._CommaSeparated . _2 . _Just

arrelems :: AsJType r ws a => Traversal' r (Elems ws a)
arrelems = _JArr . _1 . _Wrapped . CS._CommaSeparated . _2 . _Just

keyLen :: HasJAssoc c ws a => L.Getter c Int
keyLen = jsonAssocKey . _Wrapped . L.to length

elemsinit :: (CS.HasElems c ws a) => L.ASetter' c ws
elemsinit = CS.elemsElems . traverse . CS.elemTrailing . fmap . _2

elemslast :: CS.HasElems c ws a => L.ASetter' c ws
elemslast = CS.elemsLast . CS.elemTrailing . _Just . _2

prettyArr :: Bool -> Int -> Int -> Json -> Json
prettyArr inline step _ = P.transformOf jsonTraversal (arr inline)
  where
    i = WS (V.replicate step Space)
    arr True  = stepaftercomma
    arr False = complicatematters

    stepaftercomma a = a
      L.& arrelems . elemsinit .~ i
      L.& arrelems . elemslast .~ i

    complicatematters = id

prettyObj :: Int -> Int -> Json -> Json
prettyObj step w = P.transformOf jsonTraversal (
  setlastvaltrailing .
  setnested .
  setheadleadingws .
  setlastelem .
  setinitelems .
  alignafterkey
  )
  where
    spaces x = V.replicate x Space
    ws' x    = WS (V.singleton NewLine <> spaces x)

    i     = ws' w
    l     = ws' (w - step)

    alignafterkey j = over (objelems . traverse) (\ja ->
        let
          kl = ja L.^. keyLen
        in
          ja L.& jsonAssocValPreceedingWS .~ (WS . spaces $ longestKey - kl)
      ) j
      where
        longestKey = maybe 1 (+1) $ L.maximumOf (objelems . L.folded . keyLen) j

    setinitelems       = objelems . elemsinit .~ i
    setlastelem        = objelems . elemslast .~ l

    setnested          = objelems . nested %~ prettyObj step (w + step)
    setlastvaltrailing = objelems . nested . immediateTrailingWS .~ l

    setheadleadingws   = _JObj . _1 . _Wrapped . CS._CommaSeparated . _1 .~ i

    nested             = CS.elemsLast . CS.elemVal . jsonAssocVal


-- |
-- A 'Control.Lens.Traversal'' over the 'a' at the given 'Text' key on a JSON object.
--
-- >>> E.simplePureEncodeNoSpaces E.json (obj & oat "c" ?~ E.runPureEncoder E.int 33)
-- "{\"c\":33,\"a\":33,\"b\":\"Fred\"}"
-- >>> E.simplePureEncodeNoSpaces E.json (obj & oat "d" ?~ E.runPureEncoder E.text "sally")
-- "{\"d\":\"sally\",\"a\":33,\"b\":\"Fred\"}"
--
oat :: (AsJType r ws a, Semigroup ws, Monoid ws) => Text -> Traversal' r (Maybe a)
oat k = _JObj . _1 . _MapLikeObj . at k

-- |
-- A 'Control.Lens.Traversal'' over the 'a' at the given 'Int' position in a JSON object.
--
-- >>> E.simplePureEncodeNoSpaces E.json (obj & oix 0 .~ E.runPureEncoder E.int 1)
-- "{\"a\":1,\"b\":\"Fred\"}"
-- >>> E.simplePureEncodeNoSpaces E.json (obj & oix 1 .~ E.runPureEncoder E.text "sally")
-- "{\"a\":33,\"b\":\"sally\"}"
oix :: (Semigroup ws, Monoid ws, AsJType r ws a) => Int -> Traversal' r a
oix i = _JObj . _1 . ix i

-- |
-- A 'Control.Lens.Traversal'' over the 'a' at the given 'Int' position in a JSON array.
--
-- >>> E.simplePureEncodeNoSpaces E.json ((E.runPureEncoder (E.list E.int) [1,2,3]) & aix 0 .~ E.runPureEncoder E.int 99)
-- "[99,2,3]"
-- >>> E.simplePureEncodeNoSpaces E.json ((E.runPureEncoder (E.list E.int) [1,2,3]) & aix 2 .~ E.runPureEncoder E.int 44)
-- "[1,2,44]"
aix :: (AsJType r ws a, Semigroup ws, Monoid ws) => Int -> Traversal' r a
aix i = _JArr . _1 . ix i


-- | Parse a 'null' value.
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
  -> f (JType ws a)
parseJNull ws = JNull
  <$ text "null"
  <*> ws

-- | Parse a 'true' or 'false'.
--
-- >>> testparse (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparse (parseJBool (return ())) "false"
-- Right (JBool False ())
--
-- >>> testparsetheneof (parseJBool (return ())) "true"
-- Right (JBool True ())
--
-- >>> testparsetheneof (parseJBool (return ())) "false"
-- Right (JBool False ())
--
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
  -> f (JType ws a)
parseJBool ws =
  let
    b q t = JBool q <$ text t
  in
    (b False "false" <|> b True "true") <*> ws

-- | Parse a JSON numeric value.
parseJNum
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType ws a)
parseJNum ws =
  JNum <$> parseJNumber <*> ws

-- | Parse a JSON string.
--
-- >>> testparse (parseJStr (return ())) "\"\""
-- Right (JStr (JString' []) ())
--
-- >>> testparse (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparse (parseJStr (return ())) "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either DecodeError (JType () a)
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"\""
-- Right (JStr (JString' []) ())
--
-- >>> testparsetheneof (parseJStr (return ())) "\"abc\""
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\\u"
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJStr (return ())) "\"a\"\t"
-- Right (JStr (JString' [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJStr
  :: CharParsing f
  => f ws
  -> f (JType ws a)
parseJStr ws =
  JStr <$> parseJString <*> ws

-- | Parse a JSON array.
parseJArr
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType ws Json)
parseJArr ws =
  JArr <$> parseJArray ws parseWaargonaut <*> ws

-- | Parse a JSON object.
parseJObj
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType ws Json)
parseJObj ws =
  JObj <$> parseJObject ws parseWaargonaut <*> ws

-- | Try to parse each of our 'JType' possibilities.
parseJType
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f (JType ws Json)
parseJType =
  asum . distribute
    [ parseJNull
    , parseJBool
    , parseJNum
    , parseJStr
    , parseJArr
    , parseJObj
    ]

-- | Using the given whitespace builder, create a builder for a given 'Json' value.
waargonautBuilder
  :: (WS -> Builder)
  -> Json
  -> Builder
waargonautBuilder ws (Json jt) =
  jTypesBuilder ws jt

-- | Parse to a 'Json' value, keeping all of the information about the leading
-- and trailing whitespace.
parseWaargonaut
  :: ( Monad f
     , CharParsing f
     )
  => f Json
parseWaargonaut =
  Json <$> parseJType parseWhitespace
