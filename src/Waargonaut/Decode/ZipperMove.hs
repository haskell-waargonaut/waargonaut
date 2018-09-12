module Waargonaut.Decode.ZipperMove
  ( ZipperMove (..)
  , AsZipperMove (..)
  , ppZipperMove
  ) where

import           Control.Lens                  (Prism')
import qualified Control.Lens                  as L

import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           Numeric.Natural               (Natural)

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL

-- |
-- Set of moves that may be executed on a zipper.
--
data ZipperMove
  = U
  | D
  | DAt Text
  | Item Text
  | L Natural
  | R Natural
  deriving (Show, Eq)

ppZipperMove :: ZipperMove -> Doc a
ppZipperMove m = case m of
  U        -> WL.text "up/"
  D        -> WL.text "into\\"

  (L n)    -> WL.text "->-" <+> ntxt n
  (R n)    -> WL.text "-<-" <+> ntxt n

  (DAt k)  -> WL.text "into\\" <+> itxt "key" k
  (Item t) -> WL.text "-::" <+> itxt "item" t
  where
    itxt t k' = WL.parens (WL.text t <+> WL.colon <+> WL.text (Text.unpack k'))
    ntxt n'   = WL.parens (WL.char 'i' <+> WL.char '+' <+> WL.text (show n'))

class AsZipperMove r where
  _ZipperMove :: Prism' r ZipperMove
  _U          :: Prism' r ()
  _D          :: Prism' r ()
  _DAt        :: Prism' r Text
  _Item       :: Prism' r Text
  _L          :: Prism' r Natural
  _R          :: Prism' r Natural

  _U    = _ZipperMove . _U
  _D    = _ZipperMove . _D
  _DAt  = _ZipperMove . _DAt
  _Item = _ZipperMove . _Item
  _L    = _ZipperMove . _L
  _R    = _ZipperMove . _R

instance AsZipperMove ZipperMove where
  _ZipperMove = id

  _U = L.prism (const U)
       (\x -> case x of
           U -> Right ()
           _ -> Left x
       )

  _D = L.prism (const D)
       (\x -> case x of
           D -> Right ()
           _ -> Left x
       )

  _DAt = L.prism DAt
         (\x -> case x of
             DAt y -> Right y
             _     -> Left x
         )

  _Item = L.prism Item
          (\x -> case x of
              Item y -> Right y
              _      -> Left x
          )

  _L = L.prism L
       (\x -> case x of
           L y -> Right y
           _   -> Left x
       )

  _R = L.prism R
       (\x -> case x of
           R y -> Right y
           _   -> Left x
       )
