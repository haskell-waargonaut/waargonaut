{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
module Waargonaut.Types.CommaSep.Elem
  (
    -- * Types
    Elem (..)
  , HasElem (..)
  , Comma (Comma)

  , _ElemTrailingIso

    -- * Parse
  , parseComma
  , parseCommaTrailingMaybe
  ) where

import           Prelude                 (Eq, Show (showsPrec), showString,
                                          shows, (&&), (==))

import           Control.Applicative     (Applicative (..), liftA2, pure, (<*>))
import           Control.Category        (id, (.))

import           Control.Lens            (Iso, Iso', Lens', from, iso, (^.))

import           Data.Bifoldable         (Bifoldable (bifoldMap))
import           Data.Bifunctor          (Bifunctor (bimap))
import           Data.Bitraversable      (Bitraversable (bitraverse))
import           Data.Foldable           (Foldable, foldMap)
import           Data.Functor            (Functor, fmap, (<$), (<$>))
import           Data.Functor.Classes    (Eq1, Show1, eq1, showsPrec1)
import           Data.Maybe              (Maybe (..), fromMaybe)
import           Data.Monoid             (Monoid (..), mempty)
import           Data.Traversable        (Traversable, traverse)

import           Data.Functor.Identity   (Identity (..))

import           Text.Parser.Char        (CharParsing)
import qualified Text.Parser.Char        as C
import qualified Text.Parser.Combinators as C

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Utils
-- >>> import Waargonaut.Types.Json
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Data.Either (Either (..))
--

-- | Unary type to represent a comma.
data Comma = Comma
  deriving (Eq, Show)

-- | Parse a single comma (,)
parseComma :: CharParsing f => f Comma
parseComma = Comma <$ C.char ','
{-# INLINE parseComma #-}

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
