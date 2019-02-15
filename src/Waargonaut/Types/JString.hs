{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions for handling JSON strings.
module Waargonaut.Types.JString
  (
    -- * Types
    JString
  , JString' (..)
  , AsJString (..)

  , _JStringText
  , stringToJString

    -- * Parser / Builder
  , parseJString
  , jStringBuilder
  ) where

import           Prelude                 (Eq, Ord, Show, String, foldr)

import           Control.Applicative     (Applicative, (*>), (<*))
import           Control.Category        (id, (.))
import           Control.Error.Util      (note)
import           Control.Lens            (Prism', Choice, Rewrapped, Wrapped (..), iso,
                                          prism, review)

import           Data.Either             (Either (Right))
import           Data.Foldable           (Foldable, foldMap)
import           Data.Function           (($))
import           Data.Functor            (Functor, fmap, (<$>))
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Traversable        (Traversable, traverse)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Digit              (HeXDigit)

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many)

import           Data.Text.Lazy.Builder  (Builder)
import qualified Data.Text.Lazy.Builder  as TB

import           Waargonaut.Types.JChar  (JChar, jCharBuilderTextL, parseJChar, jCharToChar, charToJChar,
                                          utf8CharToJChar)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((#))
-- >>> import Control.Monad (return)
-- >>> import Data.Function (($))
-- >>> import Data.Either(Either (..), isLeft)
-- >>> import Data.Digit (HeXDigit(..))
-- >>> import qualified Data.Vector as V
-- >>> import Utils
-- >>> import Waargonaut.Decode.Error (DecodeError)
-- >>> import Waargonaut.Types.Whitespace
-- >>> import Waargonaut.Types.JChar.Unescaped
-- >>> import Waargonaut.Types.JChar.Escaped
-- >>> import Waargonaut.Types.JChar.HexDigit4
-- >>> import Waargonaut.Types.JChar
-- >>> import Data.Text.Lazy.Builder (toLazyText)
----

-- | A JSON string is a list of JSON acceptable characters, we use a newtype to
-- create the 'JString' type from a 'Vector JChar'. This is polymorphic over the
-- acceptable types of character encoding digits.
newtype JString' digit =
  JString' (Vector (JChar digit))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | As only one subset of digits are currently acceptable, Hexadecimal, we
-- provide this type alias to close that loop.
type JString = JString' HeXDigit

instance JString' digit ~ t => Rewrapped (JString' digit) t

instance Wrapped (JString' digit) where
  type Unwrapped (JString' digit) = Vector (JChar digit)
  _Wrapped' = iso (\ (JString' x) -> x) JString'

-- | Classy 'Control.Lens.Prism'' for things that may be treated as a 'JString'.
class AsJString a where
  _JString :: Prism' a JString

instance AsJString JString where
  _JString = id

instance AsJString [JChar HeXDigit] where
  _JString = prism (\(JString' cs) -> V.toList cs) (Right . JString' . V.fromList)

instance AsJString String where
  _JString = prism
    (\(JString' cx) -> V.toList $ jCharToChar <$> cx)
    (\x -> JString' . V.fromList <$> traverse (note x . charToJChar) x)

-- | Prism between a 'JString' and 'Text'.
--
-- JSON strings a wider range of encodings than 'Text' and to be consistent with
-- the 'Text' type, these invalid types are replaced with a placeholder value.
--
_JStringText :: (Choice p, Applicative f) => p Text (f Text) -> p JString (f JString)
_JStringText = iso (Text.pack . review _JString) (JString' . V.fromList . fmap utf8CharToJChar . Text.unpack)

-- | Parse a 'JString', storing escaped characters and any explicitly escaped
-- character encodings '\uXXXX'.
--
-- >>> testparse parseJString "\"\""
-- Right (JString' [])
--
-- >>> testparse parseJString "\"\\\\\""
-- Right (JString' [EscapedJChar ReverseSolidus])
--
-- >>> testparse parseJString "\"abc\""
-- Right (JString' [UnescapedJChar (Unescaped 'a'),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\""
-- Right (JString' [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either DecodeError JString
-- Right (JString' [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (Unescaped 'd'),UnescapedJChar (Unescaped 'e'),UnescapedJChar (Unescaped 'f'),EscapedJChar QuotationMark])
--
-- >>> testparsethennoteof parseJString "\"a\"\\u"
-- Right (JString' [UnescapedJChar (Unescaped 'a')])
--
-- >>> testparsethennoteof parseJString "\"a\"\t"
-- Right (JString' [UnescapedJChar (Unescaped 'a')])
parseJString
  :: CharParsing f
  => f JString
parseJString =
  char '"' *> (JString' . V.fromList <$> many parseJChar) <* char '"'

-- | Builder for a 'JString'.
--
-- >>> toLazyText $ jStringBuilder ((JString' V.empty) :: JString)
-- "\"\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')]) :: JString)
-- "\"abc\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c')]) :: JString)
-- "\"a\\rbc\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (Unescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (Unescaped 'b'),UnescapedJChar (Unescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (Unescaped 'd'),UnescapedJChar (Unescaped 'e'),UnescapedJChar (Unescaped 'f'),EscapedJChar QuotationMark]) :: JString)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.singleton (UnescapedJChar (Unescaped 'a'))) :: JString)
-- "\"a\""
--
-- >>> toLazyText $ jStringBuilder (JString' $ V.singleton (EscapedJChar ReverseSolidus) :: JString)
-- "\"\\\\\""
--
jStringBuilder
  :: JString
  -> Builder
jStringBuilder (JString' jcs) =
  TB.singleton '\"' <> foldMap jCharBuilderTextL jcs <> TB.singleton '\"'

-- | Convert a 'String' to a 'JString'.
stringToJString :: String -> JString
stringToJString = JString' . foldr (V.cons . utf8CharToJChar) V.empty

