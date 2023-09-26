module Web.Htmx where

import Data.Text (Text, pack, toLower)

data Selector
  = Class Text
  | Id Text

selectorText :: Selector -> Text
selectorText (Class t) = "." <> t
selectorText (Id t) = "#" <> t

class ToAttribute a where
  toAtt :: a -> Text

data HxTarget
  = This
  | Closest Selector
  | Find Selector
  | Next Selector
  | Previous Selector
  | Query Selector

instance ToAttribute HxTarget where
  toAtt This = "this"
  toAtt (Closest s) = "closest " <> selectorText s
  toAtt (Find s) = "find " <> selectorText s
  toAtt (Next s) = "next " <> selectorText s
  toAtt (Previous s) = "previous " <> selectorText s
  toAtt (Query s) = selectorText s

instance ToAttribute Selector where
  toAtt = selectorText

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
