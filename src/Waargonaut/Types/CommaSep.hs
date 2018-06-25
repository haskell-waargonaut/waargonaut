{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
--
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE FlexibleContexts       #-}
module Waargonaut.Types.CommaSep
  ( CommaSeparated (..)

  , Elems (..)
  , HasElems (..)

  , Elem (..)
  , HasElem (..)

  , Comma (..)
  , parseComma
  , commaBuilder
  , parseCommaSeparated
  , commaSeparatedBuilder

  , _CommaSeparated
  , toList
  ) where

import           Prelude                 (Eq, Int, Show, otherwise, (&&), (<=),
                                          (==))

import           Control.Applicative     (liftA2, pure, (*>), (<*), (<*>))
import           Control.Category        (id, (.))

import           Control.Lens            (Index, Iso, Iso', IxValue, Ixed (..),
                                          Lens', iso, snoc, to, traverse, (%%~),
                                          (^.), (^..), _2)

import           Control.Monad           (Monad)

import           Data.Char               (Char)
import           Data.Foldable           (Foldable, asum, foldMap, length)
import           Data.Function           (const, ($), (&))
import           Data.Functor            (Functor, fmap, (<$), (<$>))
import           Data.Maybe              (Maybe (..), maybe)
import           Data.Monoid             (mempty)
import           Data.Semigroup          ((<>))
import           Data.Traversable        (Traversable)
import           Data.Tuple              (snd, uncurry)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Functor.Identity   (Identity (..))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Text.Parser.Char        (CharParsing, char)
import qualified Text.Parser.Combinators as C

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Control.Applicative (Applicative, pure)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Text.Parsec (ParsecT, ParseError)
-- >>> import Text.Parser.Char (CharParsing, alphaNum)
-- >>> import Waargonaut.Types.Whitespace (WS (..), Whitespace (..), parseWhitespace)
-- >>> let charWS = ((,) <$> alphaNum <*> parseWhitespace) :: CharParsing f => f (Char, WS)
----

data Comma = Comma
  deriving (Eq, Show)

_Comma :: Iso' Comma ()
_Comma = iso (\Comma -> ()) (const Comma)

data Elem f ws a = Elem
  { _elemVal      :: a
  , _elemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

class HasElem c f ws a | c -> f ws a where
  elem :: Lens' c (Elem f ws a)
  elemTrailing :: Lens' c (f (Comma, ws))
  {-# INLINE elemTrailing #-}
  elemVal :: Lens' c a
  {-# INLINE elemVal #-}
  elemTrailing = elem . elemTrailing
  elemVal =  elem . elemVal

instance HasElem (Elem f ws a) f ws a where
 {-# INLINE elemTrailing #-}
 {-# INLINE elemVal #-}
 elem = id
 elemTrailing f (Elem x1 x2) = Elem x1 <$> f x2
 elemVal f (Elem x1 x2) = (`Elem` x2) <$> f x1

deriving instance (Show ws, Show a) => Show (Elem Identity ws a)
deriving instance (Show ws, Show a) => Show (Elem Maybe ws a)

deriving instance (Eq ws, Eq a) => Eq (Elem Identity ws a)
deriving instance (Eq ws, Eq a) => Eq (Elem Maybe ws a)

data Elems ws a = Elems
  { _elemsElems :: Vector (Elem Identity ws a)
  , _elemsLast  :: Elem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CommaSeparated ws a
  = CommaSeparated ws (Maybe (Elems ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

_CommaSeparated :: Iso (CommaSeparated ws a) (CommaSeparated ws' b) (ws, Maybe (Elems ws a)) (ws', Maybe (Elems ws' b))
_CommaSeparated = iso (\(CommaSeparated ws a) -> (ws,a)) (uncurry CommaSeparated)

type instance IxValue (CommaSeparated ws a) = a
type instance Index (CommaSeparated ws a) = Int

instance Ixed (CommaSeparated ws a) where

  ix _ _ c@(CommaSeparated _ Nothing) = pure c

  ix i f c@(CommaSeparated w (Just es))
    | i == 0 && es ^. elemsElems . to V.null =
      CommaSeparated w . Just <$> (es & elemsLast . traverse %%~ f)
    | i <= es ^. elemsElems . to length =
      CommaSeparated w . Just <$> (es & elemsElems . ix i . traverse %%~ f)
    | otherwise = pure c

class HasElems c ws a | c -> ws a where
  elems :: Lens' c (Elems ws a)
  elemsElems :: Lens' c (Vector (Elem Identity ws a))
  {-# INLINE elemsElems #-}
  elemsLast :: Lens' c (Elem Maybe ws a)
  {-# INLINE elemsLast #-}
  elemsElems = elems . elemsElems
  elemsLast = elems . elemsLast
instance HasElems (Elems ws a) ws a where
  {-# INLINE elemsElems #-}
  {-# INLINE elemsLast #-}
  elems = id
  elemsElems f (Elems x1 x2) = fmap (`Elems` x2) (f x1)
  elemsLast f (Elems x1 x2) = fmap (Elems x1) (f x2)

toList :: CommaSeparated ws a -> [a]
toList = maybe [] g . (^. _CommaSeparated . _2)
  where
    g e = snoc
      (e ^.. elemsElems . traverse . elemVal)
      (e ^. elemsLast . elemVal)

commaBuilder :: Builder
commaBuilder = BB.charUtf8 ','

parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','

-- |
--
-- >>> testparse (parseCommaTrailingMaybe parseWhitespace) ", "
-- Right (Just (Comma,WS [Space]))
--
-- >>> testparse (parseCommaTrailingMaybe parseWhitespace) " , "
-- Right Nothing
--
-- >>> testparse (parseCommaTrailingMaybe parseWhitespace) ",, "
-- Right (Just (Comma,WS []))
--
parseCommaTrailingMaybe
  :: CharParsing f
  => f ws
  -> f (Maybe (Comma, ws))
parseCommaTrailingMaybe =
  C.optional . liftA2 (,) parseComma

commaTrailingBuilder
  :: Foldable f
  => (ws -> Builder)
  -> f (Comma, ws)
  -> Builder
commaTrailingBuilder wsB =
  foldMap ((commaBuilder <>) . wsB . snd)

commaSeparatedBuilder
  :: forall ws a. Char
  -> Char
  -> (ws -> Builder)
  -> (a -> Builder)
  -> CommaSeparated ws a
  -> Builder
commaSeparatedBuilder op fin wsB aB (CommaSeparated lws sepElems) =
  BB.charUtf8 op <> wsB lws <> maybe mempty buildElems sepElems <> BB.charUtf8 fin
  where
    elemBuilder
      :: Foldable f
      => Elem f ws a -> Builder
    elemBuilder (Elem e eTrailing) =
      aB e <> commaTrailingBuilder wsB eTrailing

    buildElems (Elems es elst) =
      foldMap elemBuilder es <> elemBuilder elst

-- |
--
-- >>> testparse (parseCommaSeparatedElems parseWhitespace alphaNum) "a, b, c, d"
-- Right (Elems {_elemsElems = [Elem {_elemVal = 'a', _elemTrailing = Identity (Comma,WS [Space])},Elem {_elemVal = 'b', _elemTrailing = Identity (Comma,WS [Space])},Elem {_elemVal = 'c', _elemTrailing = Identity (Comma,WS [Space])}], _elemsLast = Elem {_elemVal = 'd', _elemTrailing = Nothing}})
--
-- >>> testparse (parseCommaSeparatedElems parseWhitespace alphaNum) "a, b,c,d, "
-- Right (Elems {_elemsElems = [Elem {_elemVal = 'a', _elemTrailing = Identity (Comma,WS [Space])},Elem {_elemVal = 'b', _elemTrailing = Identity (Comma,WS [])},Elem {_elemVal = 'c', _elemTrailing = Identity (Comma,WS [])}], _elemsLast = Elem {_elemVal = 'd', _elemTrailing = Just (Comma,WS [Space])}})
--
-- >>> testparse (parseCommaSeparatedElems parseWhitespace alphaNum) "d, "
-- Right (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = 'd', _elemTrailing = Just (Comma,WS [Space])}})
--
-- >>> testparse (parseCommaSeparatedElems parseWhitespace charWS) "d , "
-- Right (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = ('d',WS [Space]), _elemTrailing = Just (Comma,WS [Space])}})
--
-- >>> testparse (parseCommaSeparatedElems parseWhitespace charWS) "d\n, e,  "
-- Right (Elems {_elemsElems = [Elem {_elemVal = ('d',WS [NewLine]), _elemTrailing = Identity (Comma,WS [Space])}], _elemsLast = Elem {_elemVal = ('e',WS []), _elemTrailing = Just (Comma,WS [Space,Space])}})
--
parseCommaSeparatedElems
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (Elems ws a)
parseCommaSeparatedElems ws a = do
  hd <- a
  sep <- parseCommaTrailingMaybe ws
  maybe (pure $ Elems mempty (Elem hd sep)) (go mempty . (hd,)) sep
  where
    idElem e = Elem e . Identity

    fin cels lj sp =
      pure $ Elems cels (Elem lj sp)

    go commaElems (lastJ, lastSep) = do
      mJ <- C.optional a
      case mJ of
        Nothing -> fin commaElems lastJ (Just lastSep)
        Just j -> do
          msep <- parseCommaTrailingMaybe ws
          let commaElems' = snoc commaElems $ idElem lastJ lastSep
          maybe (fin commaElems' j Nothing) (go commaElems' . (j,)) msep

-- |
--
-- >>> testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[]"
-- Right (CommaSeparated (WS []) Nothing)
--
-- >>> testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[ ]"
-- Right (CommaSeparated (WS [Space]) Nothing)
--
-- >>> isLeft $ testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[ , ]"
-- True
--
-- >>> isLeft $ testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[ , a]"
-- True
--
-- >>> isLeft $ testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[d a]"
-- True
--
-- >>> testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[d , ]"
-- Right (CommaSeparated (WS []) (Just (Elems {_elemsElems = [], _elemsLast = Elem {_elemVal = ('d',WS [Space]), _elemTrailing = Just (Comma,WS [Space])}})))
--
-- >>> testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[\na\n , b]"
-- Right (CommaSeparated (WS [NewLine]) (Just (Elems {_elemsElems = [Elem {_elemVal = ('a',WS [NewLine,Space]), _elemTrailing = Identity (Comma,WS [Space])}], _elemsLast = Elem {_elemVal = ('b',WS []), _elemTrailing = Nothing}})))
--
-- >>> testparse (parseCommaSeparated (char '[') (char ']') parseWhitespace charWS) "[\na\n , b, \n]"
-- Right (CommaSeparated (WS [NewLine]) (Just (Elems {_elemsElems = [Elem {_elemVal = ('a',WS [NewLine,Space]), _elemTrailing = Identity (Comma,WS [Space])}], _elemsLast = Elem {_elemVal = ('b',WS []), _elemTrailing = Just (Comma,WS [Space,NewLine])}})))
--
parseCommaSeparated
  :: ( Monad f
     , CharParsing f
     )
  => f open
  -> f close
  -> f ws
  -> f a
  -> f (CommaSeparated ws a)
parseCommaSeparated op fin ws a =
  op *> (
    CommaSeparated <$> ws <*> asum
      [ Nothing <$ fin
      , Just <$> parseCommaSeparatedElems ws a <* fin
      ]
  )
