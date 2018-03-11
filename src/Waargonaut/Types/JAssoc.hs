module Waargonaut.Types.JAssoc where

import Waargonaut.Types.JString (JString)

data JAssoc digit s = JAssoc
  { _key   :: LeadingTrailing (JString digit) s
  , _value :: LeadingTrailing (Json digit s) s
  }
  deriving (Eq, Ord, Show)
makeClassy       ''JAssoc

instance Functor (JAssoc digit) where
    fmap f (JAssoc k v) = JAssoc (fmap f k) ((\x -> x{_a = fmap f (_a x)}) . fmap f $ v)

instance Foldable (JAssoc digit) where
    foldMap f (JAssoc k v) = mconcat [foldMap f k, foldMap' v] where
        foldMap' (LeadingTrailing l x r) = mconcat [f l, foldMap f x, f r]

instance Traversable (JAssoc digit) where
    traverse f (JAssoc k v) = JAssoc <$> traverse f k <*> traverse' v where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

parseJAssoc ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JAssoc digit s)
parseJAssoc s =
  JAssoc <$> parseLeadingTrailing s parseJString Applicative.<* char ':' <*> parseLeadingTrailing s (parseJson s)
