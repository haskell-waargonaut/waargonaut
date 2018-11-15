{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Both arrays and objects in JSON allow for an optional trailing comma on the
-- final element. This module houses the shared types and functions that let us
-- handle this.
module Waargonaut.Types.CommaSep
  (
    -- * Types
    CommaSeparated (..)
  , Elems (..)
  , HasElems (..)
  , Elem (..)
  , HasElem (..)
  , Comma (..)

    -- * Parse / Build
  , parseComma
  , commaBuilder
  , parseCommaSeparated
  , commaSeparatedBuilder

    -- * Conversion
  , _CommaSeparated
  , toList
  , fromList

    -- * Cons / Uncons
  , consCommaSep
  , unconsCommaSep
  ) where

import           Prelude                 (Eq, Int, Show (showsPrec),
                                          showString, shows, (&&), (==), (||))

import           Control.Applicative     (Applicative (..), liftA2, pure, (*>),
                                          (<*), (<*>))
import           Control.Category        (id, (.))

import           Control.Lens            (AsEmpty (..), Cons (..), Index, Iso,
                                          Iso', IxValue, Ixed (..), Lens',
                                          Snoc (..), cons, from, isn't, iso,
                                          mapped, nearly, over, prism, snoc, to,
                                          traverse, unsnoc, (%%~), (%~), (.~),
                                          (^.), (^..), (^?), _1, _2, _Cons,
                                          _Just, _Nothing)

import           Control.Error.Util      (note)
import           Control.Monad           (Monad)

import           Data.Bifoldable         (Bifoldable (bifoldMap))
import           Data.Bifunctor          (Bifunctor (bimap))
import           Data.Bitraversable      (Bitraversable (bitraverse))
import           Data.Char               (Char)
import           Data.Either             (Either (..))
import           Data.Foldable           (Foldable, asum, foldMap, foldr,
                                          length)
import           Data.Function           (const, flip, ($), (&))
import           Data.Functor            (Functor, fmap, (<$), (<$>))
import           Data.Functor.Classes    (Eq1, Show1, eq1, showsPrec1)
import           Data.Maybe              (Maybe (..), fromMaybe, maybe)
import           Data.Monoid             (Monoid (..), mempty)
import           Data.Semigroup          (Semigroup ((<>)))
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
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Text.Parser.Char (CharParsing, alphaNum)
-- >>> import Waargonaut.Types.Whitespace (WS (..), Whitespace (..), parseWhitespace)
-- >>> let charWS = ((,) <$> alphaNum <*> parseWhitespace) :: CharParsing f => f (Char, WS)
----

-- | Unary type to represent a comma.
data Comma = Comma
  deriving (Eq, Show)

-- | Isomorphism for 'Comma'.
_Comma :: Iso' Comma ()
_Comma = iso (\Comma -> ()) (const Comma)

-- | Builder for UTF8 Comma
commaBuilder :: Builder
commaBuilder = BB.charUtf8 ','
{-# INLINE commaBuilder #-}

-- | Parse a single comma (,)
parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','
{-# INLINE parseComma #-}


-- | Data type to represent a single element in a 'CommaSeparated' list. Carries
-- information about it's own trailing whitespace. Denoted by the 'f'.
data Elem f ws a = Elem
  { _elemVal      :: a
  , _elemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

instance (Monoid ws, Applicative f) => Applicative (Elem f ws) where
  pure a = Elem a (pure (Comma, mempty))
  (Elem atob _) <*> (Elem a t') = Elem (atob a) t'

instance Functor f => Bifunctor (Elem f) where
  bimap f g (Elem a t) = Elem (g a) (fmap (fmap f) t)

instance Foldable f => Bifoldable (Elem f) where
  bifoldMap f g (Elem a t) = g a `mappend` foldMap (foldMap f) t

instance Traversable f => Bitraversable (Elem f) where
  bitraverse f g (Elem a t) = Elem <$> g a <*> traverse (traverse f) t

-- | Typeclass for things that contain a single 'Elem' structure.
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

instance (Show1 f, Show ws, Show a) => Show (Elem f ws a) where
  showsPrec _ (Elem v t) =
    showString "Elem {_elemVal = " . shows v .
      showString ", _elemTrailing = " . showsPrec1 0 t . showString "}"

instance (Eq1 f, Eq ws, Eq a) => Eq (Elem f ws a) where
  Elem v1 t1 == Elem v2 t2 = v1 == v2 && eq1 t1 t2

floopId :: Monoid ws => Iso' (Identity (Comma,ws)) (Maybe (Comma,ws))
floopId = iso (Just . runIdentity) (pure . fromMaybe (Comma, mempty))

_ElemTrailingIso
  :: ( Monoid ws
     , Monoid ws'
     )
  => Iso (Elem Identity ws a) (Elem Identity ws' a') (Elem Maybe ws a) (Elem Maybe ws' a')
_ElemTrailingIso = iso
  (\(Elem a t) -> Elem a (t ^. floopId))
  (\(Elem a t) -> Elem a (t ^. from floopId))

-- | This type represents a non-empty list of elements, enforcing that the any
-- element but the last must be followed by a trailing comma and supporting option
-- of a final trailing comma.
data Elems ws a = Elems
  { _elemsElems :: Vector (Elem Identity ws a)
  , _elemsLast  :: Elem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor Elems where
  bimap f g (Elems es el) = Elems (fmap (bimap f g) es) (bimap f g el)

instance Bifoldable Elems where
  bifoldMap f g (Elems es el) = foldMap (bifoldMap f g) es `mappend` bifoldMap f g el

instance Bitraversable Elems where
  bitraverse f g (Elems es el) = Elems <$> traverse (bitraverse f g) es <*> bitraverse f g el

-- | Typeclass for things that contain an 'Elems' structure.
class HasElems c ws a | c -> ws a where
  elems      :: Lens' c (Elems ws a)
  elemsElems :: Lens' c (Vector (Elem Identity ws a))
  {-# INLINE elemsElems #-}
  elemsLast  :: Lens' c (Elem Maybe ws a)
  {-# INLINE elemsLast #-}
  elemsElems = elems . elemsElems
  elemsLast  = elems . elemsLast

instance HasElems (Elems ws a) ws a where
  {-# INLINE elemsElems #-}
  {-# INLINE elemsLast #-}
  elems = id
  elemsElems f (Elems x1 x2) = fmap (`Elems` x2) (f x1)
  elemsLast f (Elems x1 x2) = fmap (Elems x1) (f x2)

instance Monoid ws => Applicative (Elems ws) where
  pure a = Elems mempty (pure a)
  Elems atobs atob <*> Elems as a = Elems (liftA2 (<*>) atobs as) (atob <*> a)

instance Monoid ws => Semigroup (Elems ws a) where
  (<>) (Elems as alast) (Elems bs blast) =
    Elems (snoc as (alast ^. from _ElemTrailingIso) <> bs) blast

-- | This type is our possibly empty comma-separated list of values. It carries
-- information about any leading whitespace before the first element, as well as a
-- the rest of the elements in an 'Elems' type.
data CommaSeparated ws a = CommaSeparated ws (Maybe (Elems ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor CommaSeparated where
  bimap f g (CommaSeparated ws c) = CommaSeparated (f ws) (fmap (bimap f g) c)

instance Bifoldable CommaSeparated where
  bifoldMap f g (CommaSeparated ws c) = f ws `mappend` foldMap (bifoldMap f g) c

instance Bitraversable CommaSeparated where
  bitraverse f g (CommaSeparated ws c) = CommaSeparated <$> f ws <*> traverse (bitraverse f g) c

-- | By ignoring whitespace we're able to write a 'Cons' instance.
instance Monoid ws => Cons (CommaSeparated ws a) (CommaSeparated ws a) a a where
  _Cons = prism (\(a,cs) -> consCommaSep ((Comma,mempty), a) cs) (\c -> note c . over (mapped . _1) (^. _2) $ unconsCommaSep c)
  {-# INLINE _Cons #-}

instance Monoid ws => Snoc (CommaSeparated ws a) (CommaSeparated ws a) a a where
  _Snoc = prism f g
    where
      f :: (CommaSeparated ws a, a) -> CommaSeparated ws a
      f (cs,a) = over (_CommaSeparated . _2 . _Just)
        (\es -> es
          & elemsElems %~ flip snoc (es ^. elemsLast . from _ElemTrailingIso)
          & elemsLast . elemVal .~ a
        ) cs

      g :: CommaSeparated ws a -> Either (CommaSeparated ws a) (CommaSeparated ws a, a)
      g c@(CommaSeparated _   Nothing) = Left c
      g   (CommaSeparated w (Just es)) = Right
        ( CommaSeparated w $ createNewElems <$> es ^? elemsElems . _Snoc
        , es ^. elemsLast . elemVal
        )
        where
          createNewElems (newEs, newL) = es
            & elemsElems .~ newEs
            & elemsLast .~ newL ^. _ElemTrailingIso

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

instance (Monoid ws, Semigroup ws) => Semigroup (CommaSeparated ws a) where
  (CommaSeparated wsA a) <> (CommaSeparated wsB b) = CommaSeparated (wsA <> wsB) (a <> b)

instance (Monoid ws, Semigroup ws) => Monoid (CommaSeparated ws a) where
  mempty = CommaSeparated mempty Nothing
  mappend = (<>)

instance Monoid ws => Filterable (CommaSeparated ws) where
  mapMaybe _ (CommaSeparated ws Nothing)              = CommaSeparated ws Nothing
  mapMaybe f (CommaSeparated ws (Just (Elems es el))) = CommaSeparated ws newElems
    where
      newElems = case traverse f el of
        Nothing -> (\(v,l) -> Elems v (l ^. _ElemTrailingIso)) <$> unsnoc (mapMaybe (traverse f) es)
        Just l' -> Just $ Elems (mapMaybe (traverse f) es) l'

instance Monoid ws => Witherable (CommaSeparated ws) where

-- | Isomorphism between the internal pieces of a 'CommaSeparated' element.
_CommaSeparated :: Iso (CommaSeparated ws a) (CommaSeparated ws' b) (ws, Maybe (Elems ws a)) (ws', Maybe (Elems ws' b))
_CommaSeparated = iso (\(CommaSeparated ws a) -> (ws,a)) (uncurry CommaSeparated)
{-# INLINE _CommaSeparated #-}

-- | Cons elements onto a 'CommaSeparated' with provided whitespace information.
-- If you don't need explicit whitespace then the 'Cons' instance is more straightforward.
consCommaSep :: Monoid ws => ((Comma,ws),a) -> CommaSeparated ws a -> CommaSeparated ws a
consCommaSep (ews,a) = over (_CommaSeparated . _2) (pure . maybe new (consElems (ews,a)))
  where new = Elems mempty (Elem a Nothing)
{-# INLINE consCommaSep #-}

-- | Attempt to "uncons" elements from the front of a 'CommaSeparated' without
-- discarding the elements' whitespace information. If you don't need explicit
-- whitespace then the 'Cons' instance is more straightforward.
unconsCommaSep :: Monoid ws => CommaSeparated ws a -> Maybe ((Maybe (Comma,ws), a), CommaSeparated ws a)
unconsCommaSep (CommaSeparated ws es) = over _2 (CommaSeparated ws) . unconsElems <$> es
{-# INLINE unconsCommaSep #-}

instance (Semigroup ws, Monoid ws) => AsEmpty (CommaSeparated ws a) where
  _Empty = nearly mempty (^. _CommaSeparated . _2 . to (isn't _Nothing))

type instance IxValue (CommaSeparated ws a) = a
type instance Index (CommaSeparated ws a)   = Int

-- | Without a notion of "keys", this list can only be indexed by 'Int'
instance Ixed (CommaSeparated ws a) where

  ix _ _ c@(CommaSeparated _ Nothing) = pure c

  ix i f (CommaSeparated w (Just es)) = CommaSeparated w . Just <$>
    if i == 0 && es ^. elemsElems . to V.null || i == es ^. elemsElems . to length
    then es & elemsLast . traverse %%~ f
    else es & elemsElems . ix i . traverse %%~ f

-- | Convert a list of 'a' to a 'CommaSeparated' list, with no whitespace.
fromList :: (Monoid ws, Semigroup ws) => [a] -> CommaSeparated ws a
fromList = foldr cons mempty

-- | Convert a 'CommaSeparated' of 'a' to @[a]@, discarding whitespace.
toList :: CommaSeparated ws a -> [a]
toList = maybe [] g . (^. _CommaSeparated . _2) where
  g e = snoc (e ^.. elemsElems . traverse . elemVal) (e ^. elemsLast . elemVal)
{-# INLINE toList #-}

-- | Parse an optional comma and its trailing whitespace.
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

-- | Builder for a comma and trailing whitespace combination.
commaTrailingBuilder
  :: Foldable f
  => (ws -> Builder)
  -> f (Comma, ws)
  -> Builder
commaTrailingBuilder wsB =
  foldMap ((commaBuilder <>) . wsB . snd)

-- | Using the given builders for the whitespace and elements ('a'), create a
-- builder for a 'CommaSeparated'.
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

-- | Parse the elements of a 'CommaSeparated' list, handling the optional trailing comma and its whitespace.
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

-- | Parse a 'CommaSeparated' data structure.
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
