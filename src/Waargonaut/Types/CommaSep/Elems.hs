{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TupleSections          #-}
module Waargonaut.Types.CommaSep.Elems
  (
    -- * Types
    Elems (..)
  , HasElems (..)

    -- * Parse
  , parseCommaSeparatedElems

    -- * Functions
  , consElems
  , unconsElems
  ) where

import           Prelude                        (Eq, Show)

import           Control.Applicative            (Applicative (..), liftA2, pure,
                                                 (<*>))
import           Control.Category               (id, (.))
import           Control.Monad                  (Monad)

import           Control.Lens                   (Lens', cons, from, snoc, to,
                                                 (%~), (.~), (^.), (^?), _Cons)

import           Data.Bifoldable                (Bifoldable (bifoldMap))
import           Data.Bifunctor                 (Bifunctor (bimap))
import           Data.Bitraversable             (Bitraversable (bitraverse))
import           Data.Foldable                  (Foldable, foldMap)
import           Data.Function                  (($), (&))
import           Data.Functor                   (Functor, fmap, (<$>))
import           Data.Functor.Identity          (Identity (..))
import           Data.Maybe                     (Maybe (..), maybe)
import           Data.Monoid                    (Monoid (..), mempty)
import           Data.Semigroup                 (Semigroup ((<>)))
import           Data.Traversable               (Traversable, traverse)

import           Data.Vector                    (Vector)

import           Text.Parser.Char               (CharParsing)
import qualified Text.Parser.Combinators        as C

import           Waargonaut.Types.CommaSep.Elem (Comma, Elem (..), HasElem (..),
                                                 parseCommaTrailingMaybe,
                                                 _ElemTrailingIso)
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Data.Digit (HeXDigit)
-- >>> import Text.Parser.Char (alphaNum)
-- >>> import Data.Char (Char)
-- >>> let charWS = ((,) <$> alphaNum <*> parseWhitespace) :: CharParsing f => f (Char, WS)
----

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
  bitraverse f g (Elems es el) = Elems
    <$> traverse (bitraverse f g) es
    <*> bitraverse f g el

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
