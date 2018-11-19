{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions for handling JSON strings.
module Waargonaut.Types.JString
  (
    -- * Types
    JString
  , JString' (..)
  , AsJString (..)

    -- * Parser / Builder
  , parseJString
  , jStringBuilder

  , textToJString
  ) where

import           Prelude                 (Eq, Ord, Show, String)

import           Control.Applicative     ((*>), (<*))
import           Control.Category        (id, (.))
import           Control.Error.Util      (note)
import           Control.Lens            (Prism', Rewrapped, Wrapped (..), iso,
                                          preview, prism, ( # ), (^?))

import           Data.Either             (Either (Right))
import           Data.Foldable           (Foldable, foldMap)
import           Data.Function           (($))
import           Data.Functor            (Functor, (<$>))
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Traversable        (Traversable, traverse)

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Digit              (HeXDigit)

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators (many)

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BS8

import           Waargonaut.Types.JChar  (JChar, jCharBuilder, parseJChar,
                                          utf8CharToJChar, _JChar)

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
  _JString = prism (\(JString' cx) -> V.toList $ (_JChar #) <$> cx) (\x -> JString' . V.fromList <$> traverse (note x . (^? _JChar)) x)

instance AsJString Text where
  _JString = prism (Text.pack . (_JString #)) (\x -> note x . preview _JString . Text.unpack $ x)

instance AsJString ByteString where
  _JString = prism (BS8.pack . (_JString #)) (\x -> note x . preview _JString . BS8.unpack $ x)

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
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' V.empty) :: JString)
-- "\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"abc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) :: JString)
-- "\"a\\rbc\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.fromList [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar (WhiteSpace CarriageReturn),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex (HexDigit4 HeXDigita HeXDigitb HeXDigit1 HeXDigit2)),EscapedJChar (WhiteSpace NewLine),UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) :: JString)
-- "\"a\\rbc\\uab12\\ndef\\\"\""
--
-- >>> BB.toLazyByteString $ jStringBuilder ((JString' $ V.singleton (UnescapedJChar (JCharUnescaped 'a'))) :: JString)
-- "\"a\""
--
-- >>> BB.toLazyByteString $ jStringBuilder (JString' $ V.singleton (EscapedJChar ReverseSolidus) :: JString)
-- "\"\\\\\""
--
jStringBuilder
  :: JString
  -> BB.Builder
jStringBuilder (JString' jcs) =
  BB.charUtf8 '\"' <> foldMap jCharBuilder jcs <> BB.charUtf8 '\"'

-- | Prism between a 'JString' and 'Text'.
--
-- JSON strings a wider range of encodings than 'Text' and to be consistent with
-- the 'Text' type, these invalid types are replaced with a placeholder value.
--
textToJString :: Text -> JString
textToJString = JString' . Text.foldr (V.cons . utf8CharToJChar) V.empty

-- _JStringText :: Prism' JString Text
-- _JStringText = prism
--   ()
--   (\j@(JString' v) -> note j $ Text.pack . V.toList <$> traverse jCharToUtf8Char v)
