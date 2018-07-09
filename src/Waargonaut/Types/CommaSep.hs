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
  , fromList
  --
  , consCommaSep
  , unconsCommaSep
  ) where

import           Prelude                 (Eq, Int, Show, otherwise, (&&), (<=),
                                          (==))

import           Control.Applicative     (Applicative (..), liftA2, pure, (*>),
                                          (<*), (<*>))
import           Control.Category        (id, (.))

import           Control.Lens            (Cons (..), Index, Iso, Iso', IxValue, AsEmpty (..),
                                          Ixed (..), Lens', cons, iso, mapped, nearly, _Nothing, isn't,
                                          over, prism, snoc, to, traverse,
                                          unsnoc, (%%~), (%~), (.~), (^.),
                                          (^..), (^?), _1, _2, _Cons)

import           Control.Error.Util      (note)
import           Control.Monad           (Monad)

import           Data.Char               (Char)
import           Data.Foldable           (Foldable, asum, foldMap, foldr,
                                          length)
import           Data.Function           (const, ($), (&))
import           Data.Functor            (Functor, fmap, (<$), (<$>))
import           Data.Maybe              (Maybe (..), fromMaybe, maybe)
import           Data.Monoid             (Monoid (..), mempty)
import           Data.Semigroup          (Semigroup (..), (<>))
import           Data.Traversable        (Traversable)
import           Data.Tuple              (snd, uncurry)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Functor.Identity   (Identity (..))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Text.Parser.Char        (CharParsing, char)
import qualified Text.Parser.Combinators as C

import           Data.Witherable         (Filterable (..), Witherable (..))

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

instance (Monoid ws, Applicative f) => Applicative (Elem f ws) where
  pure a = Elem a (pure (Comma, mempty))
  -- Should I try to combine the trailing or just disregard one? Unsure if this is a lawful instance.
  (Elem atob _) <*> (Elem a t') = Elem (atob a) t'

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

flipGInLast
  :: Monoid ws
  => Elem Identity ws a
  -> Elem Maybe ws a
flipGInLast (Elem a t) =
  Elem a (Just $ runIdentity t)

flipFInLast
  :: Monoid ws
  => Elem Maybe ws a
  -> Elem Identity ws a
flipFInLast (Elem a t) =
  Elem a (Identity $ fromMaybe (Comma, mempty) t)

data Elems ws a = Elems
  { _elemsElems :: Vector (Elem Identity ws a)
  , _elemsLast  :: Elem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Monoid ws => Applicative (Elems ws) where
  pure a = Elems mempty (pure a)
  Elems atobs atob <*> Elems as a = Elems (liftA2 (<*>) atobs as) (atob <*> a)

instance Monoid ws => Semigroup (Elems ws a) where
  (<>) (Elems as alast) (Elems bs blast) = Elems (snoc as (flipFInLast alast) <> bs) blast

data CommaSeparated ws a = CommaSeparated ws (Maybe (Elems ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Monoid ws => Cons (CommaSeparated ws a) (CommaSeparated ws a) a a where
  _Cons = prism (\(a,cs) -> consCommaSep ((Comma,mempty), a) cs) (\c -> note c . over (mapped . _1) (^. _2) $ unconsCommaSep c)
  {-# INLINE _Cons #-}

instance (Monoid ws, Semigroup ws) => Monoid (CommaSeparated ws a) where
  mempty = CommaSeparated mempty Nothing
  mappend (CommaSeparated wsA a) (CommaSeparated wsB b) = CommaSeparated (wsA <> wsB) (a <> b)

instance Monoid ws => Filterable (CommaSeparated ws) where
  mapMaybe _ (CommaSeparated ws Nothing)              = CommaSeparated ws Nothing
  mapMaybe f (CommaSeparated ws (Just (Elems es el))) = CommaSeparated ws newElems
    where
      newElems = case traverse f el of
        Nothing -> (\(v,l) -> Elems v (flipGInLast l)) <$> unsnoc (mapMaybe (traverse f) es)
        Just l' -> Just $ Elems (mapMaybe (traverse f) es) l'

instance Monoid ws => Witherable (CommaSeparated ws) where

_CommaSeparated :: Iso (CommaSeparated ws a) (CommaSeparated ws' b) (ws, Maybe (Elems ws a)) (ws', Maybe (Elems ws' b))
_CommaSeparated = iso (\(CommaSeparated ws a) -> (ws,a)) (uncurry CommaSeparated)
{-# INLINE _CommaSeparated #-}

consElems :: Monoid ws => ((Comma,ws), a) -> Elems ws a -> Elems ws a
consElems (ews,a) e = e & elemsElems %~ cons (Elem a (Identity ews))
{-# INLINE consElems #-}

unconsElems :: Monoid ws => Elems ws a -> ((Maybe (Comma,ws), a), Maybe (Elems ws a))
unconsElems e = maybe (e', Nothing) (\(em, ems) -> (idT em, Just $ e & elemsElems .~ ems)) es'
  where
    es'   = e ^? elemsElems . _Cons
    e'    = (e ^. elemsLast . elemTrailing, e ^. elemsLast . elemVal)
    idT x = (x ^. elemTrailing . to (Just . runIdentity), x ^. elemVal)
{-# INLINE unconsElems #-}

consCommaSep :: Monoid ws => ((Comma,ws),a) -> CommaSeparated ws a -> CommaSeparated ws a
consCommaSep (ews,a) = over (_CommaSeparated . _2) (pure . maybe new (consElems (ews,a)))
  where new = Elems mempty (Elem a Nothing)
{-# INLINE consCommaSep #-}

unconsCommaSep :: Monoid ws => CommaSeparated ws a -> Maybe ((Maybe (Comma,ws), a), CommaSeparated ws a)
unconsCommaSep (CommaSeparated ws es) = over _2 (CommaSeparated ws) . unconsElems <$> es
{-# INLINE unconsCommaSep #-}

type instance IxValue (CommaSeparated ws a) = a
type instance Index (CommaSeparated ws a)   = Int

instance (Semigroup ws, Monoid ws) => AsEmpty (CommaSeparated ws a) where
  _Empty = nearly mempty (^. _CommaSeparated . _2 . to (isn't _Nothing))

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

fromList :: (Monoid ws, Semigroup ws) => [a] -> CommaSeparated ws a
fromList = foldr cons mempty

toList :: CommaSeparated ws a -> [a]
toList = maybe [] g . (^. _CommaSeparated . _2) where
  g e = snoc (e ^.. elemsElems . traverse . elemVal) (e ^. elemsLast . elemVal)
{-# INLINE toList #-}

commaBuilder :: Builder
commaBuilder = BB.charUtf8 ','
{-# INLINE commaBuilder #-}

parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','
{-# INLINE parseComma #-}

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
