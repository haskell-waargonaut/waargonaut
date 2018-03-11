module Waargonaut.Types.JObject where

import Waargonaut.Types.JAssoc (JAssoc)

newtype JObject digit s = JObject
  { _jobjectL :: [LeadingTrailing (JAssoc digit s) s]
  } deriving (Eq, Ord, Show)
makeClassy       ''JObject
makeWrapped      ''JObject

instance Functor (JObject digit) where
    fmap f (JObject ls) = JObject (fmap fmap' ls) where
        fmap' (LeadingTrailing l x r) =
            LeadingTrailing (f l) (fmap f x) (f r)

instance Foldable (JObject digit) where
    foldMap f (JObject ls) = mconcat (fmap (foldMap f) ls)

instance Traversable (JObject digit) where
    traverse f (JObject ls) = JObject <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

parseJObject ::
  (Monad f, CharParsing f, HeXaDeCiMaL digit) =>
  f s
  -> f (JObject digit s)
parseJObject s =
  JObject <$>
    (
      char '{' Applicative.*>
      sepBy (parseLeadingTrailing s (parseJAssoc s)) (char ',') Applicative.<*
      char '}'
    )
