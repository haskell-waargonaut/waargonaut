{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
--
-- Some high level prisms for interacting with something that could be JSON.
--
module Waargonaut.Lens
  (
    -- * Prisms
    _TextJson
  , _Number
  , _String
  , _Bool
  , _ArrayOf
  , _ObjHashMapOf
  , _Null
  ) where

import           Prelude                         (Bool, Show)

import           Control.Applicative             (liftA2)
import           Control.Category                ((.))
import           Control.Error.Util              (note)
import           Control.Lens                    (Prism', cons, preview, prism,
                                                  review, (^?), _1, _Wrapped)
import           Control.Monad                   (Monad, void)
import           Data.Foldable                   (foldr)
import           Data.Function                   (const, ($))
import           Data.Functor                    (fmap)
import           Data.Scientific                 (Scientific)
import           Data.Tuple                      (uncurry)

import           Data.Bifunctor                  (first)
import           Data.Either                     (Either (..))

import           Text.Parser.Char                (CharParsing)

import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as TL

import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V

import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HM

import qualified Waargonaut.Types.JObject.JAssoc as JA

import qualified Waargonaut.Types.CommaSep       as CS
import           Waargonaut.Types.JString        (_JStringText)

import           Waargonaut.Types.JNumber        (_JNumberScientific)
import           Waargonaut.Types.Json           (AsJType (..), Json)

import qualified Waargonaut.Decode               as D
import qualified Waargonaut.Encode               as E

-- | 'Prism'' between 'Json' and 'Text'
_TextJson
  :: ( CharParsing g
     , Monad g
     , Show e
     )
  => (forall a. g a -> Text -> Either e a)
  -> Prism' Text Json
_TextJson pf = prism
  (TL.toStrict . E.simplePureEncodeText E.json)
  (\b -> first (const b) $ D.pureDecodeFromText pf D.json b)
{-# INLINE _TextJson #-}

-- | 'Prism'' between some 'Json' and a 'Scientific' value
_Number  :: Prism' Json Scientific
_Number = prism (E.asJson' E.scientific) (\j -> note j $ j ^? _JNum . _1 . _JNumberScientific)
{-# INLINE _Number #-}

-- | 'Prism'' between some 'Json' and a 'Text' value
_String :: Prism' Json Text
_String = prism (E.asJson' E.text) (\j -> note j $ j ^? _JStr . _1 . _JStringText)
{-# INLINE _String #-}

-- | 'Prism'' between some 'Json' and a '()' value
_Null :: Prism' Json ()
_Null = prism (E.asJson' E.null) (\j -> note j . void $ j ^? _JNull)
{-# INLINE _Null #-}

-- | 'Prism'' between some 'Json' and a 'Bool' value
_Bool :: Prism' Json Bool
_Bool = prism (E.asJson' E.bool) (\j -> note j $ j ^? _JBool . _1)
{-# INLINE _Bool#-}

-- | 'Prism'' between some 'Json' and an array of something given the provided 'Prism''
_ArrayOf :: Prism' Json x -> Prism' Json (Vector x)
_ArrayOf _Value = prism fromJ toJ
  where
    fromJ = E.asJson' (E.traversable E.json) . fmap (review _Value)
    {-# INLINE fromJ #-}
    toJ = CS.fromCommaSep (_JArr . _1 . _Wrapped) V.empty (foldr cons V.empty) (preview _Value)
    {-# INLINE toJ #-}
{-# INLINE _ArrayOf #-}

-- | 'Prism'' between some 'Json' and a strict 'HashMap' with 'Text' keys, and
-- some value of a type provided by the given @Prism' Json x@.
_ObjHashMapOf :: Prism' Json x -> Prism' Json (HashMap Text x)
_ObjHashMapOf _Value = prism toJ fromJ
  where
    toJ = E.asJson' (E.keyValueTupleFoldable (E.prismE _Value E.json)) . HM.toList
    {-# INLINE toJ #-}

    toVals el = liftA2 (,)
      (preview (JA.jsonAssocKey . _JStringText) el)
      (preview (JA.jsonAssocVal . _Value) el)
    {-# INLINE toVals #-}

    fromJ = CS.fromCommaSep (_JObj . _1 . _Wrapped) HM.empty
      (foldr (uncurry HM.insert) HM.empty) toVals
    {-# INLINE fromJ #-}
{-# INLINE _ObjHashMapOf #-}
