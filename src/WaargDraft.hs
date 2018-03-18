{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module WaargDraft where

import           Prelude                  (Eq, Ord, Show, (==))

import           Control.Applicative      (pure)
import           Control.Category         ((.))
import           Control.Lens             (Index, IxValue, Ixed (..),
                                           Plated (..), makeClassy,
                                           makeClassyPrisms, makeWrapped, to,
                                           traverse, (%%~), (^.), _Wrapped)

import           Data.Foldable            (Foldable)
import           Data.Traversable         (Traversable)

import           Data.Function            (($), (&))
import           Data.Functor             (Functor (..))

import           Data.Bool                (Bool)
import           Waargonaut.Types.JNumber
import           Waargonaut.Types.JString


data Wrap s a = Wrap
  { _leadingWS  :: s
  , _inner      :: a
  , _trailingWS :: s
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype J digit s = J
  { _unJ :: Wrap s (JBit digit s)
  }
  deriving (Eq, Ord, Show)

-- | JSON Array
newtype JBits digit s = JBits
  { _unBits :: [J digit s]
  } deriving (Eq, Ord, Show)

-- | Associated values, HashMap that cares about leading/trailing @Whitespace@
data JBAssoc digit s = JBAssoc
  { _jBAssocKey   :: Wrap s (JString digit)
  , _jBAssocValue :: J digit s
  }
  deriving (Eq, Ord, Show)

-- | JSON Object
newtype JBObject digit s = JBObject
  { _JBObjectL :: [Wrap s (JBAssoc digit s)]
  } deriving (Eq, Ord, Show)

data JBit digit s
  = JBitNull
  | JBitBool Bool
  | JBitNumber JNumber
  | JBitString (JString digit)
  | JBitArray (JBits digit s)
  | JBitObject (JBObject digit s)
  deriving (Eq, Ord, Show)

makeClassy ''Wrap

makeWrapped ''J
makeClassy ''J

makeClassy ''JBits
makeClassy ''JBAssoc
makeClassy ''JBObject

makeClassyPrisms ''JBit

type instance Index (J digit s)   = JString digit
type instance IxValue (J digit s) = J digit s

instance Eq digit => Ixed (J digit s) where
  ix i f = _Wrapped . inner . _JBitObject . jBObjectL . traverse . traverse %%~ \w ->
    if w ^. jBAssocKey . inner . to (== i)
    then w & jBAssocValue %%~ f
    else pure w

instance Plated (JBit digit s) where
  plate _    JBitNull      = pure JBitNull
  plate _ b@(JBitBool _)   = pure b
  plate _ n@(JBitNumber _) = pure n
  plate _ s@(JBitString _) = pure s

  plate f (JBitArray bs)   =
    fmap JBitArray $ bs & unBits . traverse . _Wrapped . inner %%~ f

  plate f (JBitObject jo)  =
    fmap JBitObject $ jo & jBObjectL . traverse . inner . jBAssocValue . _Wrapped . inner %%~ f
