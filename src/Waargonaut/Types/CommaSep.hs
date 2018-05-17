{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections     #-}
--
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Waargonaut.Types.CommaSep where

import           Control.Applicative     (liftA2)
import           Control.Lens            (snoc)

import           Data.Semigroup          ((<>))

import           Data.Traversable        (Traversable)

import           Data.Foldable           (asum)

import           Data.Functor            (Functor)
-- import           Data.Functor.Classes    (Eq1 (..), Show1 (..), eq1, showsPrec1)
import           Data.Functor.Identity   (Identity (..))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

import           Text.Parser.Char        (CharParsing, char)
import qualified Text.Parser.Combinators as C

data Comma = Comma
  deriving (Eq, Show)

data Elem f ws a = Elem
  { _elemVal      :: a
  , _elemTrailing :: f (Comma, ws)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Show ws, Show a) => Show (Elem Identity ws a)
deriving instance (Show ws, Show a) => Show (Elem Maybe ws a)

deriving instance (Eq ws, Eq a) => Eq (Elem Identity ws a)
deriving instance (Eq ws, Eq a) => Eq (Elem Maybe ws a)

-- instance (Show ws, Show1 f) => Show1 (Elem f ws) where
--   liftShowsPrec = liftShowsPrec

-- instance (Show a, Show ws, Show1 f) => Show (Elem f ws a) where
--   showsPrec = showsPrec1

-- instance (Eq ws, Eq1 f) => Eq1 (Elem f ws) where
--   liftEq = liftEq

-- instance (Eq a, Eq ws, Eq1 f) => Eq (Elem f ws a) where
--   (==) = eq1

data CommaSeparated ws a
  = CommaSeparated ws (Maybe (Elems ws a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Elems ws a = Elems
  { _elems     :: [Elem Identity ws a]
  , _elemsLast :: Elem Maybe ws a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

commaB :: Builder
commaB = BB.charUtf8 ','

parseComma :: CharParsing f => f Comma
parseComma = Comma <$ char ','

parseCommaTrailingMaybe
  :: CharParsing f
  => f ws
  -> f (Maybe (Comma, ws))
parseCommaTrailingMaybe =
  C.optional . liftA2 (,) parseComma

parseCommaTrailingIdentity
  :: CharParsing f
  => f ws
  -> f (Identity (Comma, ws))
parseCommaTrailingIdentity ws =
  Identity <$> liftA2 (,) parseComma ws

commaTrailingBuilder
  :: ( Functor f
     , Foldable f
     )
  => (ws -> Builder)
  -> f (Comma, ws)
  -> Builder
commaTrailingBuilder wsB =
  foldMap ((commaB <>) . wsB . snd)

commaSeparatedBuilder
  :: Char
  -> Char
  -> (ws -> Builder)
  -> (a -> Builder)
  -> CommaSeparated ws a
  -> Builder
commaSeparatedBuilder op fin wsB aB (CommaSeparated lws elems) =
  BB.charUtf8 op <> wsB lws <> maybe mempty buildElems elems <> BB.charUtf8 fin
  where
    elemBuilder (Elem e eTrailing) =
      aB e <> commaTrailingBuilder wsB eTrailing

    buildElems (Elems es elst) = foldMap elemBuilder es <> elemBuilder elst

parseCommaSepOpTrailing
  :: ( Monad f
     , CharParsing f
     )
  => (e -> (Comma,ws) -> b)
  -> (e -> Maybe (Comma, ws) -> c)
  -> ([b] -> c -> d)
  -> f ws
  -> f e
  -> [b]
  -> (e, (Comma, ws))
  -> f d
parseCommaSepOpTrailing idElem mayElem outerCons fws fa = go
  where
    fin cels lj sp =
      pure $ outerCons cels (mayElem lj sp)

    go commaElems (lastJ, lastSep) = do
      mJ <- C.optional fa
      case mJ of
        Nothing -> fin commaElems lastJ (Just lastSep)
        Just j -> do
          msep <- parseCommaTrailingMaybe fws
          let commaElems' = snoc commaElems $ idElem lastJ lastSep
          maybe (fin commaElems' j Nothing) (go commaElems' . (j,)) msep

parseCommaSeparatedElems
  :: ( Monad f
     , CharParsing f
     )
  => f ws
  -> f a
  -> f (Elems ws a)
parseCommaSeparatedElems ws a = do
  let
    idElem e = Elem e . Identity

  hd <- a
  sep <- parseCommaTrailingMaybe ws
  maybe
    (pure $ Elems [] (Elem hd sep))
    (parseCommaSepOpTrailing idElem Elem Elems ws a [] . (hd,))
    sep

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
