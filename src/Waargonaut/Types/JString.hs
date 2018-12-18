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

import           Control.Applicative        ((*>), (<*))
import           Control.Category           (id, (.))
import           Control.Error.Util         (note)
import           Control.Lens               (Prism', Rewrapped, Wrapped (..),
                                             iso, prism, review, ( # ), (^?))

import           Data.Bifunctor             (first)
import           Data.Either                (Either (Right))
import           Data.Foldable              (Foldable, foldMap)
import           Data.Function              (const, ($))
import           Data.Functor               (Functor, fmap, (<$>))
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Traversable           (Traversable, traverse)

import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import           Data.Digit                 (HeXDigit)

import           Text.Parser.Char           (CharParsing, char)
import           Text.Parser.Combinators    (many)

import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as TB

import qualified Data.ByteString.Lazy.Char8 as BS8

import           Waargonaut.Types.JChar     (JChar, jCharBuilderTextL,
                                             parseJChar, utf8CharToJChar,
                                             _JChar)

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
    (\(JString' cx) -> V.toList $ (_JChar #) <$> cx)
    (\x -> JString' . V.fromList <$> traverse (note x . (^? _JChar)) x)

-- | Prism between a 'JString' and 'Text'.
--
-- JSON strings a wider range of encodings than 'Text' and to be consistent with
-- the 'Text' type, these invalid types are replaced with a placeholder value.
--
_JStringText :: Prism' JString Text
_JStringText = prism
  (JString' . V.fromList . fmap utf8CharToJChar . Text.unpack)
  (\x -> first (const x) . Text.decodeUtf8' . BS8.toStrict . BS8.pack . review _JString $ x)

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
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\""
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\"" :: Either DecodeError JString
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark])
--
-- >>> testparsethennoteof parseJString "\"a\"\\u"
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a')])
--
-- >>> testparsethennoteof parseJString "\"a\"\t"
-- Right (JString' [UnescapedJChar (JCharUnescaped 'a')])
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
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"abc\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"a\\rbc\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) :: JString)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> toLazyText $ jStringBuilder ((JString' $ V.singleton (UnescapedJChar (JCharUnescaped 'a'))) :: JString)
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

