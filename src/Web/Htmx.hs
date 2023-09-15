module Web.Htmx where

import Data.Text (Text, pack, toLower)

newtype Selector
  = Class Text

selectorText :: Selector -> Text
selectorText (Class t) = "." <> t

class ToAttribute a where
  toAtt :: a -> Text

data HxTarget
  = This
  | Closest Selector
  | Find Selector
  | Next Selector
  | Previous Selector

instance ToAttribute HxTarget where
  toAtt This = "this"
  toAtt (Closest s) = "closest " <> selectorText s
  toAtt (Find s) = "find " <> selectorText s
  toAtt (Next s) = "next " <> selectorText s
  toAtt (Previous s) = "previous " <> selectorText s

-- TODO: run into trouble with constructor clashes! Might need to use fns again
data HxSwap
  = InnerHTML
  | OuterHTML
  | BeforeBegin
  | AfterBegin
  | BeforeEnd
  | AfterEnd
  | Delete
  | None
  deriving (Show)

instance ToAttribute HxSwap where
  toAtt InnerHTML = "innerHMTL"
  toAtt OuterHTML = "outerHTML"
  toAtt a = toLower . pack . show $ a
