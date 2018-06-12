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
{-# LANGUAGE TemplateHaskell        #-}
module Waargonaut.Types.CommaSep
  ( CommaSeparated (..)

  , Elems (..)
  , HasElems (..)

  , Elem (..)
  , Comma (..)
  , parseComma
  , commaBuilder
  , parseCommaSeparated
  , commaSeparatedBuilder

  , _CommaSeparated
  ) where

import           Prelude                 (Eq, Show)

import           Control.Applicative     (liftA2, pure, (*>), (<*), (<*>))
import           Control.Category        ((.))
import           Control.Lens            (Iso', Prism', iso, makeClassy, prism',
                                          snoc)
import           Control.Monad           (Monad)

import           Data.Char               (Char)
import           Data.Foldable           (Foldable, asum, foldMap)
import           Data.Function           (const, ($))
import           Data.Functor            (Functor, (<$), (<$>))
import           Data.Maybe              (Maybe (..), maybe)
import           Data.Monoid             (mempty)
import           Data.Semigroup          ((<>))
import           Data.Traversable        (Traversable)
import           Data.Tuple              (snd, uncurry)

import           Data.Functor.Identity   (Identity (..))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Text.Parser.Char        (CharParsing, char)
import qualified Text.Parser.Combinators as C

data Comma = Comma
  deriving (Eq, Show)

_Comma :: Prism' Comma ()
_Comma = prism' (const Comma) (\Comma -> Just ())

data Elem f ws a = Elem
  { _elemVal      :: a
  , _elemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

makeClassy ''Elem

deriving instance (Show ws, Show a) => Show (Elem Identity ws a)
deriving instance (Show ws, Show a) => Show (Elem Maybe ws a)

deriving instance (Eq ws, Eq a) => Eq (Elem Identity ws a)
deriving instance (Eq ws, Eq a) => Eq (Elem Maybe ws a)

data CommaSeparated ws a
  = CommaSeparated ws (Maybe (Elems ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

_CommaSeparated :: Iso' (CommaSeparated ws a) (ws, Maybe (Elems ws a))
_CommaSeparated = iso (\(CommaSeparated ws a) -> (ws,a)) (uncurry CommaSeparated)

data Elems ws a = Elems
  { _elemsElems :: [Elem Identity ws a]
  , _elemsLast  :: Elem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeClassy ''Elems

commaBuilder :: Builder
commaBuilder = BB.charUtf8 ','

parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','

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
  :: Char
  -> Char
  -> (ws -> Builder)
  -> (a -> Builder)
  -> CommaSeparated ws a
  -> Builder
commaSeparatedBuilder op fin wsB aB (CommaSeparated lws sepElems) =
  BB.charUtf8 op <> wsB lws <> maybe mempty buildElems sepElems <> BB.charUtf8 fin
  where
    elemBuilder (Elem e eTrailing) =
      aB e <> commaTrailingBuilder wsB eTrailing

    buildElems (Elems es elst) = foldMap elemBuilder es <> elemBuilder elst

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
  maybe (pure $ Elems [] (Elem hd sep)) (go [] . (hd,)) sep
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
