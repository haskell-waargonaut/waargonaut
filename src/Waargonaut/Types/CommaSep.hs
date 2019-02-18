{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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

    -- * Parse
  , parseComma
  , parseCommaSeparated

    -- * Conversion
  , _CommaSeparated
  , toList
  , fromList

    -- * Cons / Uncons
  , consCommaSep
  , unconsCommaSep
  ) where

import           Prelude                         (Eq, Int, Show, (&&), (==),
                                                  (||))

import           Control.Applicative             (Applicative (..), pure, (*>),
                                                  (<*), (<*>))
import           Control.Category                ((.))

import           Control.Lens                    (AsEmpty (..), Cons (..),
                                                  Index, Iso, IxValue,
                                                  Ixed (..), Snoc (..), cons,
                                                  from, iso, mapped, nearly,
                                                  over, prism, snoc, to,
                                                  traverse, unsnoc, (%%~), (%~),
                                                  (.~), (^.), (^..), (^?), _1,
                                                  _2, _Cons, _Just, _Nothing)
import           Control.Lens.Extras             (is)

import           Control.Error.Util              (note)
import           Control.Monad                   (Monad)

import           Data.Bifoldable                 (Bifoldable (bifoldMap))
import           Data.Bifunctor                  (Bifunctor (bimap))
import           Data.Bitraversable              (Bitraversable (bitraverse))
import           Data.Either                     (Either (..))
import           Data.Foldable                   (Foldable, asum, foldMap,
                                                  foldr, length)
import           Data.Function                   (flip, ($), (&))
import           Data.Functor                    (Functor, fmap, (<$), (<$>))
import           Data.Maybe                      (Maybe (..), maybe)
import           Data.Monoid                     (Monoid (..), mempty)
import           Data.Semigroup                  (Semigroup ((<>)))
import           Data.Traversable                (Traversable)
import           Data.Tuple                      (uncurry)

import qualified Data.Vector                     as V

import           Text.Parser.Char                (CharParsing)

import           Data.Witherable                 (Filterable (..),
                                                  Witherable (..))

import           Waargonaut.Types.CommaSep.Elem  (Comma (..), Elem (..),
                                                  HasElem (..), parseComma,
                                                  _ElemTrailingIso)

import           Waargonaut.Types.CommaSep.Elems (Elems (..), HasElems (..),
                                                  consElems,
                                                  parseCommaSeparatedElems,
                                                  unconsElems)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Control.Monad (return)
-- >>> import Data.Either (Either (..), isLeft)
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Data.Digit (HeXDigit)
-- >>> import Text.Parser.Char (alphaNum, char)
-- >>> let charWS = ((,) <$> alphaNum <*> parseWhitespace) :: CharParsing f => f (Char, WS)
----

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
  _Cons = prism
          (\(a,cs) -> consCommaSep ((Comma,mempty), a) cs)
          (\c -> note c . over (mapped . _1) (^. _2) $ unconsCommaSep c)
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
  _Empty = nearly mempty (^. _CommaSeparated . _2 . to (is _Nothing))

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
